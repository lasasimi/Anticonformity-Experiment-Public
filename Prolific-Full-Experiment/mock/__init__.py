from otree.api import *
import numpy as np # type: ignore
import json
import time
import random
import pandas as pd # type: ignore

doc = """
Grouping participants with different group sizes and anticonformists to participate in a synchronous discussion task.
"""
def open_CSV(filename):
    """
    :param filename: opinions.csv
    :return: a list of statements for the session
    """
    temp = pd.read_csv(filename, sep=',')
    return temp


class C(BaseConstants):
    NAME_IN_URL = 'mock'
    PLAYERS_PER_GROUP = None

    # NOTE: Replace with 20 for real experiment
    NUM_ROUNDS = 20 
    # NOTE: Set this to 20 minutes
    LONG_WAIT = 20  #(minutes)
    # NOTE: Set this to 19 minutes for longer wait time, or 15 minutes for shorter wait time
    MEDIUM_WAIT = 19 # (minutes) # IF NO GROUP OF 8 HAS BEEN FORMED, CREATE A GROUP OF 4

    # No changes below
    N_TEST = 8 # SIZE OF DISCUSSION GROUP 
    MAX_WARNING = 3 # Max number of forced responses before the inactivity warning shows up
    MAX_FORCED = NUM_ROUNDS // 2  # Max number of forced responses to not get paid
    SCENARIOS = open_CSV('presurvey/scenarios_1np.csv')
    
    # NOTE: Max number of groups in each condition is set up in session config

    # Custom admin view fields
    ADMIN_VIEW_FIELDS = {
        'player': ['round_number', 'scenario', 'old_response', 'new_response', 'forced_response', 'prev_majority', 'neighbor_responses', 'discussion_grp', 'aff_pol_A', 'aff_pol_F'],
        'group': ['group_size', 'is_group_single', 'beta_50', 'anti_prop', 'group_responses', 'majority_response'],
    }
 

class Subsession(BaseSubsession):
    pass

def creating_session(subsession):
    session = subsession.session
    # Retrieve values from session config and store them in the session
    session.MAX_N04_p00 = session.config.get('MAX_N04_p00', 0)
    session.MAX_N04_p100 = session.config.get('MAX_N04_p100', 0)
    session.MAX_N04_p50 = session.config.get('MAX_N04_p50', 0)
    session.MAX_N08_p00 = session.config.get('MAX_N08_p00', 0)
    session.MAX_N08_p100 = session.config.get('MAX_N08_p100', 0)
    session.MAX_N08_p50 = session.config.get('MAX_N08_p50', 0)
    # Control condition
    session.MAX_N08_p99 = session.config.get('MAX_N08_p99', 0) 
    session.MAX_N04_p99 = session.config.get('MAX_N04_p99', 0)
    session.SCE = session.config.get('SCE')
    session.start_time = time.time()  # record the session start time

    # initialize the counters
    for player in subsession.get_players():
        player.participant.away_long = False
        player.participant.forced_response_remaining = C.MAX_WARNING
        player.participant.control = False
        player.participant.too_many_forced = False
        player.participant.forced_response_counter = 0
        player.participant.positive = False

def N08_full(subsession):
    session = subsession.session
    return (
    session.N08_p00 == session.MAX_N08_p00 and
    session.N08_p100 == session.MAX_N08_p100 and
    session.N08_p50 == session.MAX_N08_p50 and
    session.N08_p99 == session.MAX_N08_p99
)

def long_wait(player):
    participant = player.participant
    # In bot tests, immediately trigger long_wait to allow single-player progression
    if participant._is_bot:
        return False
    return time.time() - participant.wait_page_arrival > C.LONG_WAIT * 60 

def long_away(player):
    participant = player.participant
    return time.time() - participant.wait_page_arrival > (C.LONG_WAIT * 60) + 35


def medium_wait(player):
    participant = player.participant
    # In bot tests, immediately trigger medium_wait to allow group formation
    # if participant._is_bot:
    #     return True
    return time.time() - participant.wait_page_arrival > C.MEDIUM_WAIT * 60 

def counters_full(player):
    session = player.subsession.session
    return (session.N04_p00 == session.MAX_N04_p00 and session.N04_p100 == session.MAX_N04_p100 and session.N04_p50 == session.MAX_N04_p50 and
            session.N08_p00 == session.MAX_N08_p00 and session.N08_p100 == session.MAX_N08_p100 and session.N08_p50 == session.MAX_N08_p50 and session.MAX_N08_p99 == session.N08_p99 and session.MAX_N04_p99 == session.N04_p99)


def group_by_arrival_time_method(subsession, waiting_players):
    session = subsession.session
    print(f'Waiting players: {waiting_players}')
    response = {}
    for p in waiting_players:
        response[p.participant.code] = p.participant.all_responses
        p.participant.scenario = session.SCE
    
    # Dynamically reconstruct scenario_counts from participant.vars
    sce = session.SCE
    scenario_counts = {sce: {'A': [], 'F': []}}
    sce_A = scenario_counts[sce]['A'] # player objects choosing -1
    sce_F = scenario_counts[sce]['F'] # player objects choosing +1

    for p in waiting_players:
        if sce in response[p.participant.code].keys():
            if response[p.participant.code][sce] == -1:
                sce_A.append(p)
            elif response[p.participant.code][sce] == 1:
                sce_F.append(p)
    print(f"Debug: Scenario counts before grouping: {scenario_counts}")

    group = []

    if len(waiting_players) == C.N_TEST and not N08_full(subsession):
    # check if creating 1 group of 8 is possible 
        print('N08 is not full, creating a group of 8')
        
        print(f"Debug: Scenario {sce}, A count: {len(sce_A)}, F count: {len(sce_F)}")
        if len(sce_A) == C.N_TEST/2 and len(sce_F) == C.N_TEST/2: 
            print('Ready to create a LARGE discussion group')
            group = sce_A + sce_F
            for p in group:
                p.participant.scenario = sce
                # Save the scenario and faction to participant.vars
                p.participant.vars['scenario'] = sce
                p.participant.vars['faction'] = 'A' if p in sce_A else 'F'
            return group 
    # If N08 is full or any of the players has been waiting for medium time, create a group of 4
    if N08_full(subsession) or any(medium_wait(p) for p in waiting_players):
        print('N08 is full or medium wait, checking for smaller groups')
        
        if len(waiting_players) >= C.N_TEST//2:
            print('Creating a group of 4')
            print(len(sce_A), len(sce_F))

            if len(sce_A) >= C.N_TEST//2:
                group = random.sample(sce_A, k=C.N_TEST//2)
            elif len(sce_F) >= C.N_TEST//2:
                group = random.sample(sce_F, k=C.N_TEST//2)
            
                print("About to assign scenarios to group:", group)
                for p in group:
                    p.participant.scenario = sce
                    # Save the scenario and faction to participant.vars
                    p.participant.vars['scenario'] = sce
                    p.participant.vars['faction'] = 'A' if p in sce_A else 'F'
                return group
            
    # Check if counters are full
    if all(counters_full(p) for p in waiting_players):
        print("All counters are full. Adding all waiting players to long_waiting.")
        long_waiting = waiting_players  # Add all players to long_waiting
    else:
        # players waiting for more than threshold need to be let go
        long_waiting = [p for p in waiting_players if long_wait(p) or counters_full(p)]

    if len(long_waiting) >= 1:
        for player in long_waiting:
            print('Ready to let participant go, waiting for too long')
            return [player]
        
    if not group:
        print('Not enough players yet to create a group')
        return []
    
    return group


class Group(BaseGroup):
    group_size = models.StringField(initial='single')
    is_group_single = models.BooleanField()
    beta_50 = models.BooleanField()  # for beta 0.50 treatment
    anti_prop = models.StringField()  # for p value treatment
    group_responses = models.LongStringField()  # store as string and later to dump as JSON
    majority_response = models.IntegerField() # to store the majority response in the final round

class Player(BasePlayer):
    scenario = models.StringField()
    discussion_grp = models.StringField()
    old_response = models.IntegerField(label="What do you think the community should do?",
                                   choices=[[-1, 'Against'],
                                            [0, 'Neutral'],
                                            [1, 'For']],
                                            widget=widgets.RadioSelectHorizontal())
    new_response = models.IntegerField(label="What do you think the community should do?",
                                   choices=[[-1, 'Against'],
                                            [0, 'Neutral'],
                                            [1, 'For']],
                                            widget=widgets.RadioSelectHorizontal())
    forced_response = models.BooleanField(initial=False)
    nudge_training =  models.IntegerField(label="If you follow the tip you just read, what would be your choice?",
                                          choices=[[-1, 'Against'],
                                                   [0, 'Neutral'],
                                                   [1, 'For']],
                                                   widget=widgets.RadioSelectHorizontal())
    nudge_training_two =  models.IntegerField(label="If you follow the tip you just read, what would be your choice?",
                                        choices=[[-1, 'Against'],
                                                [0, 'Neutral'],
                                                [1, 'For']],
                                                widget=widgets.RadioSelectHorizontal())
    nudge_training_three =  models.IntegerField(label="If you follow the tip you just read, what would be your choice?",
                                        choices=[[-1, 'Against'],
                                                [0, 'Neutral'],
                                                [1, 'For']],
                                                widget=widgets.RadioSelectHorizontal())
    nudge_training_text = models.StringField() # to show what they selected
    correct_nudge_training = models.IntegerField()
    old_response_text = models.StringField() # to show previous response in the popup
    # Attention check
    attention_check=models.BooleanField(initial=False)
    # Majority
    prev_majority = models.IntegerField()
    # Neighbor responses
    neighbor_responses = models.StringField()
    # Additional measure of affective polarization
    aff_pol_A = models.IntegerField(
        label="What feelings do you have towards people in this study who chose to",
        min=0,
        max=100,
    )
    aff_pol_F = models.IntegerField(
        label="What feelings do you have towards people in this study who chose to",
        min=0,
        max=100,
    )
                                                                                                                  
    
def counters_update(group:Group):
    if group.group_size == 'N08':
        if group.anti_prop == 'p00':
            group.subsession.session.N08_p00 += 1
        if group.anti_prop == 'p100':
            group.subsession.session.N08_p100 += 1
        if group.anti_prop == 'p50':
            group.subsession.session.N08_p50 += 1
        if group.anti_prop == 'p99':
            group.subsession.session.N08_p99 += 1
    if group.group_size == 'N04':
        if group.anti_prop == 'p00':
            group.subsession.session.N04_p00 += 1
        if group.anti_prop == 'p100':
            group.subsession.session.N04_p100 += 1
        if group.anti_prop == 'p50':
            group.subsession.session.N04_p50 += 1
        if group.anti_prop == 'p99':
            group.subsession.session.N04_p99 += 1

def p_00(group:Group):
    group.anti_prop = 'p00'
    counters_update(group)

def p_100(group:Group):
    group.anti_prop = 'p100'
    counters_update(group)

def p_50(group:Group):
    group.anti_prop = 'p50'
    counters_update(group)

def p_99(group:Group):
    group.anti_prop = 'p99'
    counters_update(group)

def random_p(group:Group):
    session = group.subsession.session
    choices = [p for p, mx in [
        ('p00', session.MAX_N08_p00 if group.group_size == 'N08' else session.MAX_N04_p00),
        ('p100', session.MAX_N08_p100 if group.group_size == 'N08' else session.MAX_N04_p100),
        ('p50', session.MAX_N08_p50 if group.group_size == 'N08' else session.MAX_N04_p50),
        ('p99', session.MAX_N08_p99 if group.group_size == 'N08' else session.MAX_N04_p99),
    ] if mx > 0]
    if not choices:
        choices = ['p00', 'p100', 'p50', 'p99']
    group.anti_prop = random.choice(choices)
    counters_update(group)


# PAGES
class GroupingWaitPage(WaitPage):
    template_name = 'mock/GroupingWaitPage.html'
    group_by_arrival_time = True
    
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey 
    
class GroupSizeWaitPage(WaitPage):
    @staticmethod
    def after_all_players_arrive(group: Group):
        session = group.subsession.session
        print('Debug: All players in the group have arrived, determining group size and treatment')
        group_players = group.get_players()
        print(f'Debug: Group players: {group_players}, number of players: {len(group_players)}')
        if len(group_players) == C.N_TEST:
            group_size = 'N08'
            is_group_single = False
        elif len(group_players) == C.N_TEST/2:
            group_size = 'N04'
            is_group_single = False
        else:
            group_size = 'single'
            is_group_single = True     

        # Save to the group model (for round 1 only)
        group.group_size = group_size
        group.is_group_single = is_group_single 

        if not group.is_group_single:
            if group.group_size == 'N08':
                group.beta_50 = True
                conditions = [
                    (session.N08_p00 < session.MAX_N08_p00, p_00),
                    (session.N08_p100 < session.MAX_N08_p100, p_100),
                    (session.N08_p50 < session.MAX_N08_p50, p_50),
                    (session.N08_p99 < session.MAX_N08_p99, p_99)]              
            elif group.group_size == 'N04':
                group.beta_50 = False
                conditions = [
                    (session.N04_p00 < session.MAX_N04_p00, p_00),
                    (session.N04_p100 < session.MAX_N04_p100, p_100),
                    (session.N04_p50 < session.MAX_N04_p50, p_50),
                    (session.N04_p99 < session.MAX_N04_p99, p_99)] 
                
            # Shuffle the order
            random.shuffle(conditions)
            # Evaluate conditions
            for condition, action in conditions:
                if condition:
                    action(group)
                    print(f'Condition: non-fallback (random choice)', f'Group beta .50?:{group.beta_50}', f'Group AC prop:{group.anti_prop}')
                    break
            else: 
                # The else is not tied to the if. It runs only if none of the conditions were True, because only then does the for loop finish without breaking.

                # if all quotas are full, choose randomly
                random_p(group)
                print("Condition: fallback (random choice)", f'Group beta .50?:{group.beta_50}', f'Group AC prop:{group.anti_prop}')

        # Save persistently to participant
        # REVIEW 
        for p in group_players:
            p.participant.group_size = group_size
            p.participant.is_group_single = is_group_single
            if group_size == 'single' and not long_away(p):
                p.participant.single_group = True  # they go to the pay app, but no bonus payment 
            if group_size == 'single' and long_away(p):
                p.participant.away_long = True  # they go to the noPay app, no payment

        print(f'Debug counter: session.N04_p00:{session.N04_p00}, session.N04_p100:{session.N04_p100}, session.N04_p50:{session.N04_p50}, session.N08_p00:{session.N08_p00}, session.N08_p100:{session.N08_p100}, session.N08_p50:{session.N08_p50}, session.N08_p99:{session.N08_p99}, session.N04_p99:{session.N04_p99}')

    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey


class DiscussionGRPWaitPage(WaitPage):
    template_name = 'mock/DiscussionGRPWaitPage.html'
    @staticmethod
    def after_all_players_arrive(group: Group):
        players = group.get_players()
        
        if group.subsession.round_number == 1:
            # Define how many anticonformists in each faction based on group parameter
            if group.anti_prop == 'p50':
                n_anti = 2
            elif group.anti_prop == 'p100':
                n_anti = 4
            elif group.anti_prop == 'p00':
                n_anti = 0
            elif group.anti_prop == 'p99':
                n_anti = 99

            print(f"Debug: n_anti = {n_anti}, group.anti_prop = {group.anti_prop}")
            # Save control as a participant variable 
            if n_anti == 99:
                for p in players:
                    p.participant.control = True
                    
            # Select participants to be anticonformists 
            if group.group_size == 'N08':
                faction_A = [p.participant.code for p in players if p.participant.all_responses[p.participant.scenario]==-1]
                faction_F = [p.participant.code  for p in players if p.participant.all_responses[p.participant.scenario]==1]

                if n_anti != 99:
                    anticonformists = random.sample(faction_A,n_anti) + random.sample(faction_F,n_anti) 
                    print(f"Debug: anticonformists = {anticonformists}")

                    # Extract participant codes from the anticonformists list
                    anticonformists_codes = anticonformists
                    print(f"Debug: anticonformists codes = {anticonformists}")

            elif group.group_size == 'N04':
                faction_U = players

                if n_anti != 99: 
                    anticonformists = random.sample(faction_U,n_anti)
                    print(f"Debug: anticonformists = {anticonformists}")

                    # Extract participant codes from the anticonformists list
                    anticonformists_codes = [p.participant.code for p in anticonformists]
                    print(f"Debug: anticonformists codes = {anticonformists_codes}")
                else:
                    anticonformists_codes = []
            else:
                anticonformists = []
            # Assign anticonformists to their participant level variable
            if n_anti != 99:
                for p in players:
                    if p.participant.code in anticonformists_codes:
                        p.participant.anticonformist = True 

            for p in players:
                scenario_position = p.participant.all_responses[p.participant.scenario]
                others = p.get_others_in_group()
                if scenario_position==-1:
                    p.participant.own_faction = [other.participant.code for other in others if other.participant.all_responses[other.participant.scenario]==-1]
                    p.participant.other_faction = [other.participant.code for other in others if other.participant.all_responses[other.participant.scenario]==1]

                elif scenario_position==1:
                    p.participant.own_faction = [other.participant.code for other in others if other.participant.all_responses[other.participant.scenario]==1]
                    p.participant.other_faction = [other.participant.code for other in others if other.participant.all_responses[other.participant.scenario]==-1] 
                
                else:
                    p.participant.own_faction = []
                    p.participant.other_faction = []

        else:
            # Copy group variable settings from round 1
            round1 = group.in_round(1)
            group.group_size = round1.group_size
            group.is_group_single = round1.is_group_single 
            group.beta_50 = round1.beta_50
            group.anti_prop = round1.anti_prop  

        if group.subsession.round_number == 1:
            for p in players:
                p.old_response = p.participant.all_responses[p.participant.scenario]
        else:
            for p in players:
                p.old_response = p.in_round(p.round_number - 1).new_response

        if group.group_size == 'N08':
            for p in players:
                factions = [random.choice(['own','other']) for i in range(int(C.N_TEST/2)-1)]
                labels, counts = np.unique(factions, return_counts=True)
                # Convert to plain str and int types
                faction_counts = {str(label): int(count) for label, count in zip(labels, counts)}
                faction_map = {
                    'own': p.participant.own_faction,
                    'other': p.participant.other_faction,
                    # Add more if needed
                }
                
                print(f"Debug: faction map {faction_map}, faction counts {faction_counts}")
                p.participant.discussion_grp = []
                for key in faction_counts.keys():
                    p.participant.discussion_grp = p.participant.discussion_grp + random.sample(faction_map[key],faction_counts[key])
                p.discussion_grp = str(p.participant.discussion_grp)

        if group.group_size == 'N04':
            for p in players:
                others = p.get_others_in_group()
                p.participant.discussion_grp = [o.participant.code for o in others]
                p.discussion_grp = str(p.participant.discussion_grp)

        for player in players:
            print(f"Debug: player's discussion group: {player.participant.discussion_grp}")

    @staticmethod
    def is_displayed(player):
        return player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long
    @staticmethod
    def vars_for_template(player):
        return dict(
            anticonformist = player.participant.anticonformist,
            control=player.participant.control
        )
class AttentionCheck(Page):
    form_model = 'player'
    form_fields = ['attention_check']
    timeout_seconds = 35 # NOTE: Set a timeout for the attention check
    @staticmethod
    def before_next_page(player, timeout_happened):
        if timeout_happened:
            player.participant.failed_attention_check = True # initially False
            player.participant.active = False
        else:
            if player.attention_check == False: # wrong answer
               player.participant.failed_attention_check = True 
               player.participant.active = False
            else: # correct answer
                player.participant.active = True

    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey and player.participant.single_group

class Phase3(Page):
    timeout_seconds = 45 # NOTE: change back to 45 for real experiment
    
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long


class Nudge(Page):
    timeout_seconds = 90 # NOTE: change back to 90 for real experiment
    form_model = 'player'
    form_fields = ['nudge_training']
    
    @staticmethod
    def vars_for_template(player: Player):
        row = C.SCENARIOS[C.SCENARIOS['code']==player.participant.scenario]
        
        return dict(
            scenario_against = 'option A',
            scenario_neutral = 'option B',
            scenario_for ='option C',
            # Some made up responses of other players' to be displayed
            others_responses = [1, -1, 1],# anticonformist should answer 0, conformist should answer 1
            anticonformist = player.participant.anticonformist,
        )
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        if timeout_happened: # choose the wrong answers randomly
            if player.participant.anticonformist:
                player.nudge_training = 1 # wrong answer
            else:
                player.nudge_training = random.choice([-1, 0])

        player.participant.nudge_training = player.nudge_training
        

    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long and not player.participant.control
    
class NudgeTraining(Page):
    form_model = 'player'
    form_fields = ['nudge_training_two']
    timeout_seconds = 90 # NOTE: change back to 90 for real experiment

    @staticmethod
    def vars_for_template(player):
        if player.participant.anticonformist:
            player.participant.correct_nudge_training = (
            player.participant.nudge_training in (0, -1))
        else:
            player.participant.correct_nudge_training = (
            player.participant.nudge_training == 1)

        if player.participant.nudge_training == 1:
            player.nudge_training_text = 'option C'
        elif player.participant.nudge_training == 0:
            player.nudge_training_text = 'option B'
        else:
            player.nudge_training_text = 'option A'

        return dict(
            correct_nudge_training = player.participant.correct_nudge_training,
            nudge_training_text = player.nudge_training_text,
            anticonformist = player.participant.anticonformist,
            others_responses = [1, -1, 1],
            scenario_against = 'option A',
            scenario_neutral = 'option B',
            scenario_for = 'option C',
        )
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        if timeout_happened: # choose the wrong answers randomly
            if player.participant.anticonformist:
                player.nudge_training_two = 1 # wrong answer
            else:
                player.nudge_training_two = random.choice([-1, 0])

        player.participant.nudge_training_two = player.nudge_training_two
    
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long and not player.participant.control

class NudgeTrainingLast(Page):
    form_model = 'player'
    form_fields = ['nudge_training_three']
    timeout_seconds = 90 # NOTE: change back to 90 for real experiment

    @staticmethod
    def vars_for_template(player):
        if player.participant.anticonformist:
            player.participant.correct_nudge_training = (
            player.participant.nudge_training_two in (0, -1))
        else:
            player.participant.correct_nudge_training = (
            player.participant.nudge_training_two == 1)

        if player.participant.nudge_training_two == 1:
            player.nudge_training_text = 'option C'
        elif player.participant.nudge_training_two == 0:
            player.nudge_training_text = 'option B'
        else:
            player.nudge_training_text = 'option A'

        return dict(
            correct_nudge_training = player.participant.correct_nudge_training,
            nudge_training_text = player.nudge_training_text,
            anticonformist = player.participant.anticonformist,
            others_responses = [1, -1, 1],
            scenario_against = 'option A',
            scenario_neutral = 'option B',
            scenario_for = 'option C',
        )
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        if timeout_happened: # choose the wrong answers randomly
            if player.participant.anticonformist:
                player.nudge_training_three = 1 # wrong answer
            else:
                player.nudge_training_three = random.choice([-1, 0])

        player.participant.nudge_training_three = player.nudge_training_three
    
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long and not player.participant.control



class Discussion(Page):
    def get_timeout_seconds(player):
        if player.round_number == 1:
            return 90  # longer seconds for the first round
        elif 2<= player.round_number <= 4:
            return 45
        else:
            return 30
        
    form_model = 'player'
    form_fields = ['new_response']

    @staticmethod
    def vars_for_template(player): 
        print(f'Scenario code in the discussion: {player.participant.scenario}')
        #print(C.SCENARIOS)
        row = C.SCENARIOS[C.SCENARIOS['code']==player.participant.scenario]

        discussion_partners = [other for other in player.get_others_in_group() if other.participant.code in player.participant.discussion_grp]   
        
        # Store the text of their previous response for the popup
        if player.old_response == 1:
            player.old_response_text = row.iloc[0]['For']
        elif player.old_response == 0:
            player.old_response_text = row.iloc[0]['Neutral']
        else:  
            player.old_response_text = row.iloc[0]['Against']

        return dict(
            scenario_title = row.iloc[0]['Title'],
            scenario_text = row.iloc[0]['Text'],
            scenario_against = row.iloc[0]['Against'],
            scenario_neutral = row.iloc[0]['Neutral'],
            scenario_for = row.iloc[0]['For'],
            others_responses = [other.old_response for other in discussion_partners if other.id_in_group != player.id_in_group],
            anticonformist = player.participant.anticonformist,
            old_response_text = player.old_response_text,
            round_number = player.round_number,
            control = player.participant.control,
            too_many_forced = player.participant.too_many_forced,
        )

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        discussion_partners = [other for other in player.get_others_in_group() if other.participant.code in player.participant.discussion_grp]
        neighbor_responses = [other.old_response for other in discussion_partners if other.id_in_group != player.id_in_group]

        # calculate majority from the discussion partners
        counts = {}
        for r in neighbor_responses:
            counts[r] = counts.get(r, 0) + 1
        max_count = max(counts.values())
        majority_responses = [val for val, cnt in counts.items() if cnt == max_count]
        
        # save majority response to player variable
        if len(majority_responses) == 1:
            prev_majority = majority_responses[0]
        else:
            prev_majority = 99 # Tie / No majority
        player.prev_majority = prev_majority
        
        # save neighbor responses as JSON string, but don't use to calculate majority
        player.neighbor_responses = json.dumps(neighbor_responses)

        if timeout_happened:
            ### TODO: REVIEW THE RULE #### 
            #player.forced_response = True # only in the last round, make them inactive
            #player.new_response = random.choice([-1, 0, 1])
            if not player.participant.control:
                # logic based on anticonformist or conformist
                if player.participant.anticonformist:
                    # take their neighbors' last response, see if there is a majority and choose whatever is not the majority
                    if len(majority_responses) == 1:    
                        if majority_responses[0] == -1:
                            player.new_response = random.choice([0,1])
                        elif majority_responses[0] == 0:
                            player.new_response = random.choice([-1,1])
                        else:
                            player.new_response = random.choice([-1,0])
                else:
                    # conformist choose the majority of their neighbors, if there is no majority, choose their previous response
                    if len(majority_responses) == 1:
                        player.new_response = majority_responses[0]
                    else:
                        player.new_response = player.old_response
            else:
                # control group choose their previous response
                player.new_response = player.old_response

            # toggle forced response and update counters
            player.forced_response = True
            player.participant.forced_response_remaining -= 1
            player.participant.forced_response_counter += 1

            if player.participant.forced_response_remaining == 0:
                player.participant.active = False 
                # reset for next round
                player.participant.forced_response_remaining = C.MAX_WARNING
        
        if player.participant.forced_response_counter >= C.MAX_FORCED:
            player.participant.too_many_forced = True

        # if inactive in the last round, redirect to noPay app
        if player.round_number == C.NUM_ROUNDS:
            if player.participant.too_many_forced or not player.participant.active:
                player.participant.complete_presurvey = False

    @staticmethod
    def is_displayed(player):
        print(f"Debug: Bot is {player.participant.code}, on round {player.round_number}")
        return player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long

    @staticmethod
    def live_method(player, data):
        if data.get('confirm_activity') is True:
            player.participant.active = 1
            player.participant.last_active = time.time()
        return {}

class FinalRoundWaitPage(WaitPage):
    @staticmethod
    def after_all_players_arrive(group: Group):
        # This code runs once for each group, after all players in the group reach the wait page
        group_responses = [p.new_response for p in group.get_players()]
        counts = {}
        for r in group_responses:
            counts[r] = counts.get(r, 0) + 1

        # save as JSON string
        group.group_responses = json.dumps(group_responses)

        max_count = max(counts.values())
        majority_responses = [val for val, cnt in counts.items() if cnt == max_count]
        if len(majority_responses) == 1:
            group.majority_response = majority_responses[0] 
        else:
            group.majority_response = 99 # Tie / No majority

    @staticmethod
    def is_displayed(player:Player):
        return player.round_number == C.NUM_ROUNDS and player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long

class Additional(Page):
    timeout_seconds = 180
    form_model = 'player'

    @staticmethod
    def _ordered_aff_pol_fields(player: Player):
        if 'additional_form_order' not in player.participant.vars:
            fields = ['aff_pol_A', 'aff_pol_F']
            random.shuffle(fields)
            player.participant.vars['additional_form_order'] = fields
        return player.participant.vars['additional_form_order']

    @staticmethod
    def get_form_fields(player: Player):
        return Additional._ordered_aff_pol_fields(player)

    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        if timeout_happened:
            player.aff_pol_A = 99
            player.aff_pol_F = 99

    @staticmethod
    def is_displayed(player):
        return player.round_number == C.NUM_ROUNDS and player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long

    @staticmethod
    def error_message(player: Player, values):
        errors = {}
        for field_name in Additional._ordered_aff_pol_fields(player):
            value = values.get(field_name)
            if value is None:
                errors[field_name] = 'Please move the slider to provide an answer.'
                continue
            if value < 0 or value > 100 or value % 10 != 0:
                errors[field_name] = 'Please choose one of these values: 0, 10, ..., 100.'
        return errors
    
    @staticmethod
    def vars_for_template(player):
        # Get scenario details
        row = C.SCENARIOS[C.SCENARIOS['code']==player.participant.scenario]
        suffix_map = {
            'aff_pol_A': row.iloc[0]['Against'].lower(),
            'aff_pol_F': row.iloc[0]['For'].lower(),
        }
        ordered_fields = Additional._ordered_aff_pol_fields(player)
        # Pass as list of dicts so the template engine can access values without variable key lookups
        aff_pol_questions = [
            {'field_name': f, 'suffix': suffix_map[f]}
            for f in ordered_fields
        ]
        return dict(
            option_against=row.iloc[0]['Against'].lower(),
            option_for=row.iloc[0]['For'].lower(),
            aff_pol_questions=aff_pol_questions,
        )

class FinalRound(Page):
    timeout_seconds = 90
    @staticmethod
    def vars_for_template(player: Player):
        # Get scenario details
        row = C.SCENARIOS[C.SCENARIOS['code']==player.participant.scenario]
        group = player.group
        group_responses = json.loads(group.group_responses)
    
        return dict(
            group_responses= group_responses,
            majority_response= group.majority_response,
            scenario_against = row.iloc[0]['Against'],
            scenario_neutral = row.iloc[0]['Neutral'],
            scenario_for = row.iloc[0]['For']
        )
    
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == C.NUM_ROUNDS and player.participant.complete_presurvey and not player.participant.single_group and not player.participant.away_long

page_sequence = [GroupingWaitPage, GroupSizeWaitPage, AttentionCheck, DiscussionGRPWaitPage, Nudge, NudgeTraining, NudgeTrainingLast, Phase3, Discussion, Additional, FinalRoundWaitPage, FinalRound]
