from otree.api import *
import pandas as pd # type: ignore
import numpy as np # type: ignore
import random 
import time

doc = """
Phase 1 with 2 days (8 timeslot options) to choose from
"""

def open_CSV(filename):
    """
    :param filename: opinions.csv
    :return: a list of statements for the session
    """
    temp = pd.read_csv(filename, sep=',')
    return temp


class C(BaseConstants):
    NAME_IN_URL = 'presurvey_part1b'
    PLAYERS_PER_GROUP = None

    # REMEMBER TO CHANGE TO POLITICAL/NON-POLITICAL FRAMING DEPENDING ON THE EXPERIMENTAL DESIGN
    CSV = open_CSV('presurvey/scenarios_1np.csv')
    SCENARIOS = CSV.to_dict(orient='records')
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


# Time slot ID map (day-first):
# 1-4 = Thu (4:00, 5:00, 6:00, 7:00 PM ET)
# 5-8 = Fri (4:00, 5:00, 6:00, 7:00 PM ET)
TIME_SLOT_LABELS = {
    '1': 'Tue, May 26 | 4:00 - 4:30 PM ET',
    '2': 'Tue, May 26 | 5:00 - 5:30 PM ET',
    '3': 'Tue, May 26 | 6:00 - 6:30 PM ET',
    '4': 'Tue, May 26 | 7:00 - 7:30 PM ET',
    '5': 'Wed, May 27 | 4:00 - 4:30 PM ET',
    '6': 'Wed, May 27 | 5:00 - 5:30 PM ET',
    '7': 'Wed, May 27 | 6:00 - 6:30 PM ET',
    '8': 'Wed, May 27 | 7:00 - 7:30 PM ET',
    # '9': 'Thu, May 21| 4:00 - 4:30 PM ET',
    # '10': 'Thu, May 21 | 5:00 - 5:30 PM ET',
    # '11': 'Thu, May 21 | 6:00 - 6:30 PM ET',
    # '12': 'Thu, May 21 | 7:00 - 7:30 PM ET',
    '99': '🚫 None of the listed slots work for me',
}


def creating_session(subsession):
    session = subsession.session
    # counter for keepin track of treatments
    session.N04_p00 = 0
    session.N04_p50 = 0
    session.N04_p100 = 0
    session.N08_p00 = 0
    session.N08_p50 = 0
    session.N08_p100 = 0
    session.N08_p99 = 0
    session.N04_p99 = 0
    session.SCE = session.config.get('SCE')
    session.start_time = time.time()  # record the session start time
    
    for player in subsession.get_players():
        # Shuffle the scenario order for each player
        if session.SCE == 's2_n':
            player.participant.vars['scenario_order'] = C.SCENARIOS[:1]
        elif session.SCE == 's2_p':
            player.participant.vars['scenario_order'] = C.SCENARIOS[1:2]
        player.participant.vars['training_attempt'] = 3 # Initialize training attempt
        player.participant.vars['failed_attention_check'] = False # Initialize attention check failure
        player.participant.vars['training_success'] = False # Initialize training success
        player.participant.vars['all_responses'] = {} # Initialize empty dictionary for all responses, will be appended on each round
        player.participant.vars['active'] = True # Initialize active status for the mock app later
        player.participant.vars['single_group'] = False
        player.participant.vars['anticonformist'] = False
        player.participant.vars['failed_commitment'] = False
        player.participant.vars['complete_presurvey'] = True # Initialize completed status, will be set to False will be set to False if Training_3 fails or timeout_happened
        player.participant.vars['eligible_notneutral'] = True # Initialize eligible status, will be set to False if neutral in all responses
        player.participant.vars['forced_response_counter'] = 0 
        player.participant.vars['nudge_training'] = None
        player.participant.vars['correct_nudge_training'] = False # To track if the participant answered the nudge training question correctly
        player.participant.vars['commit_phase2'] = False 
        player.participant.vars['training_wrong_question_keys'] = []


def get_training_wrong_question_keys(player: 'Player'):
    checks = {
        'dilemmatopic': player.dilemmatopic is True,
        'majority': player.majority is True,
        'howmanyneighbors': player.howmanyneighbors is True,
    }
    return [field_name for field_name, is_correct in checks.items() if not is_correct]

class Group(BaseGroup):
    pass


class Player(BasePlayer):
    gives_consent = models.BooleanField(
        label="I acknowledge that I have read and understood the information above "
        "and confirm that I wish to participate in this study.")
    scenario_code = models.StringField()
    dropout = models.BooleanField(initial=False)
    audio_unlocked = models.BooleanField(initial=False)
    # Demographics
    age = models.IntegerField(label='How old are you?', min=18, max=100)
    gender = models.StringField(choices=[['Man', 'Man'], 
                                         ['Woman', 'Woman'],
                                         ['Other', 'Other'],
                                         ['Prefer not to say', 'Prefer not to say']],
                                         label='What is your gender?',
                                         widget=widgets.RadioSelect,)
    
    education_lvl = models.StringField(
        choices=[['Less than high school', 'Less than high school'],
                ['High school diploma or equivalent', 'High school diploma or equivalent'],
                ['University degree or more', 'University degree or more']],
        label='What is the highest level of education you have completed?',
        widget=widgets.RadioSelect,)

    neighborhood_type = models.StringField(
        choices=[['Urban', 'Urban'],
                ['Suburban', 'Suburban (residential neighbourhood on the outskirts of a city or town)'],
                ['Countryside', 'Countryside (rural areas)']],
        label='What type of neighborhood do you live in?',
        widget=widgets.RadioSelect,)
    political_affiliation = models.StringField(label="Here is a 7-point scale on which the political views that people might hold are arranged from extremely liberal to extremely conservative. Where would you place yourself on this scale, or haven't you thought much about this? ",
        choices=[['1', '1 - Extremely Liberal'],
                 ['2', '2 - Liberal'],
                 ['3', '3 - Slightly Liberal'],
                 ['4', '4 - Moderate, Middle of Road'],
                 ['5', '5 - Slightly Conservative'],
                 ['6', '6 - Conservative'],
                 ['7', '7 - Extremely Conservative'],
                 ['99', 'Don\'t Know Haven\'t Thought']],
                 widget=widgets.RadioSelectHorizontal())
    # AudioCheck
    audio_answer = models.IntegerField(label="What sound did you hear?",
                                       widget=widgets.RadioSelectHorizontal,
                                        choices=[[1, 'rat noises'],
                                                [2, 'lion roars'],
                                                [3, 'rock tumbles'],
                                                [4, 'wave crashes'], # correct answer
                                                [5, 'bird chirps'],
                                                [99, '🚫 no sound, I couldn\'t hear the audio']],)
    audio_answer_image = models.IntegerField(label="What image represents the sound that you hear?",
                                             choices=[    [1,'Option 1'],
                                                            [2,'Option 2'],
                                                            [3,'Option 3'],
                                                            [4,'Option 4'],
                                                            [5,'Option 5'],
                                                            [6,'Option 6'],
                                                            [7,'Option 7'],
                                                            [8,'Option 8'],
                                                            [9,'Option 9'],
                                                        [99, '🚫 no sound, I couldn\'t hear the audio']],)
    
    # Commit phase 2
    commit_phase2 = models.BooleanField(
        label="Do you want to participate in the second phase of the experiment?",
        choices=[[True, 'Yes, I can attend the second phase in at least in one of the timeslots above.'],
                [False, 'No, I cannot attend the second phase.']],)

    time_selection = models.LongStringField(
        label="Please select at least 1 option (you may select as many as you are available for).",
        blank=True)
    
    #Time slot ID map (day-first): 
    # 1-4 Fri, 5-8 Sat, 99 = none of the listed slots. 
    timeslot_1 = models.IntegerField(initial=0)
    timeslot_2 = models.IntegerField(initial=0)
    timeslot_3 = models.IntegerField(initial=0)
    timeslot_4 = models.IntegerField(initial=0)
    timeslot_5 = models.IntegerField(initial=0)
    timeslot_6 = models.IntegerField(initial=0)
    timeslot_7 = models.IntegerField(initial=0)
    timeslot_8 = models.IntegerField(initial=0)
    # timeslot_9 = models.IntegerField(initial=0)
    # timeslot_10 = models.IntegerField(initial=0)
    # timeslot_11 = models.IntegerField(initial=0)
    # timeslot_12 = models.IntegerField(initial=0)

    
    def time_selection_error_message(player, value):
        if not value:
            return 'Please select at least one time slot.'
        # Handle both list and string formats
        if isinstance(value, list):
            selected = value
        else:
            selected = [s.strip() for s in value.split(',') if s.strip()]
        if len(selected) < 1:
            return 'Please select at least one time slot.'
        if '99' in selected and len(selected) > 1:
            return 'If you cannot attend any of the time slots, please select only that option.'
    

    # Scenario response
    response = models.IntegerField(label="What do you think the community should do?",
                                   choices=[[-1, 'Against'],
                                           [0, 'Neutral'],
                                           [1, 'For']],
                                  widget=widgets.RadioSelectHorizontal())

    # Training variables
    test_scenario = models.StringField(
    label="What do you think the community should do?",
    choices=[[-1, 'Against'],
            [0, 'Neutral'],
            [1, 'For']])
 
    dilemmatopic = models.BooleanField(
        label="The dilemma is about what the community should do with the lost cat.",
        choices=[
            [True, 'True'],
            [False, 'False'],
        ])
    majority = models.BooleanField(
        label="The majority of your neighbors are in favor of helping to search for the cat near the sewage.",
        choices=[
            [True, 'True'], 
            [False, 'False'],
        ])
    howmanyneighbors = models.BooleanField(
        label="You have 2 neighbors in your group.",
        choices=[
            [False, 'True'],# inverse the values since the correct answer is False
            [True, 'False'],
        ])

    training_counter = models.IntegerField(initial=0) # Counter for training attempts, used to limit the number of attempts
    # Attention check
    attention_check = models.IntegerField(
    label="Which of the following is a vegetable?",
    choices=[
        [1, 'Salmon'],
        [2, 'Broccoli'],
        [3, 'Cheeseburger'],
        [4, 'Pizza'],
        [5, 'Milk'],
    ],
    widget=widgets.RadioSelectHorizontal,)
    
    # Covariates
    def make_field(label):
        return models.IntegerField(
            choices=[[1, 'Strongly Disagree'],
            [2, 'Disagree'],
            [3, 'Neutral'],
            [4, 'Agree'],
            [5, 'Strongly Agree']],
            label=label,
            widget=widgets.RadioSelect,
            #blank=True, # Remove this to make it mandatory, this is done for testing purposes
        )
    
    political_charge = make_field("The situation described in the dilemma is politically controversial.")
    emotional_charge = make_field("The situation described in the dilemma is emotionally charged.")
    
                 
    # Commitment questions
    commit_attention_Q1 = models.BooleanField(
        label="I commit to giving this study my full and undivided attention.",
        choices=[[True, 'Agree'], 
                 [False, 'Disagree']],
        )
    commit_attention_Q2 = models.BooleanField(
        label="I will remain at my browser tab/window and will not open other tabs or windows during the experiment.",
        choices=[[True, 'Agree'], 
             [False, 'Disagree']],
    )
    commit_attention_Q3 = models.BooleanField(
        label="I am aware that abandoning the study prematurely could impede the other participants' ability to successfully complete it.",
        choices=[[True, 'Agree'], 
             [False, 'Disagree']],
    )


# PAGES
class Introduction(Page):
    form_model = 'player'
    form_fields = ['gives_consent', 'commit_phase2']

    @staticmethod
    def is_displayed(player:Player):
        return player.round_number == 1 
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        player.participant.gives_consent = player.gives_consent
        player.participant.commit_phase2 = player.commit_phase2
        if player.participant.gives_consent and player.participant.commit_phase2:
            player.participant.active = True# Assigning active status based on consent
        else:
            player.participant.active = False
            player.participant.complete_presurvey = False 
            player.participant.commit_phase2 = False
            
class TimeSelection(Page):
    form_model = 'player'
    form_fields = ['time_selection']

    @staticmethod
    def vars_for_template(player: Player):
        saved_value = player.field_maybe_none('time_selection') or ''
        saved_values = [item.strip() for item in saved_value.split(',') if item.strip()]
        return dict(
            saved_time_selection=saved_values,
            saved_time_selection_csv=saved_value,
        )

    @staticmethod
    def is_displayed(player:Player):
        return player.round_number == 1 and player.participant.complete_presurvey
    
    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        # Save the selected time slots as a comma-separated string
        selected_time_slots = player.time_selection
        if isinstance(selected_time_slots, list):
            player.time_selection = ','.join(selected_time_slots)

        selected_slot_ids = [s.strip() for s in (player.time_selection or '').split(',') if s.strip()]

        # Save selected slots for downstream Reminder page display
        selected_slot_ids_for_display = sorted(
            [s for s in selected_slot_ids if s in TIME_SLOT_LABELS and s != '99'],
            key=lambda s: int(s),
        )
        player.participant.vars['selected_time_slot_ids'] = selected_slot_ids_for_display
        player.participant.vars['selected_time_slots_display'] = [
            TIME_SLOT_LABELS[s] for s in selected_slot_ids_for_display
        ]

        # Create one-hot encoding for all numeric slots from TIME_SLOT_LABELS (ignore 99)
        slot_keys = [k for k in TIME_SLOT_LABELS if k != '99']
        selected_slot_set = {s for s in selected_slot_ids if s in slot_keys}
        one_hot_by_slot = {slot: (1 if slot in selected_slot_set else 0) for slot in slot_keys}

        # Store one-hot values directly on Player so they appear in standard oTree exports
        for slot in slot_keys:
            setattr(player, f'timeslot_{slot}', one_hot_by_slot[slot])
        
        # if answer is 99 (cannot attend any time slot), set commit_phase2 to False and complete_presurvey to False since they are not eligible for the main experiment
        if player.time_selection and '99' in player.time_selection.split(','):
            player.participant.commit_phase2 = False
            player.participant.complete_presurvey = False
        else:
            player.participant.commit_phase2 = True
            player.participant.complete_presurvey = True


class AudioCheck(Page):
    form_model = 'player'
    form_fields = ['audio_answer', 'audio_answer_image', 'audio_unlocked']

    @staticmethod
    def before_next_page(player, timeout_happened):
        player.participant.audio_unlocked = player.audio_unlocked
        if player.audio_answer !=4 or player.audio_answer_image !=5: # incorrect answer
            player.participant.gives_consent = False
            player.participant.complete_presurvey = False  # Assigning active status based on consent

    @staticmethod
    def vars_for_template(player:Player):
        return dict(
            mp3_url='presurvey/static/test.mp3',
        )
    
    @staticmethod
    def js_vars(player: Player):
        return dict(
            is_bot=player.participant._is_bot
        )

    @staticmethod
    def is_displayed(player:Player):
        if player.participant.commit_phase2 == False:
            return False
        else:
            return player.round_number == 1 and player.participant.gives_consent
        
    ## Give option to return the submission if the audio check is failed
    
class Demographics(Page):
    form_model = 'player'
    form_fields = ['age', 'gender','education_lvl', 'neighborhood_type', 'political_affiliation']

    @staticmethod
    def is_displayed(player:Player):
        return player.round_number == 1 and player.participant.complete_presurvey


class NeighborhoodInstruction(Page):
    @staticmethod
    def is_displayed(player:Player):
        return player.round_number == 1 and player.participant.complete_presurvey 
    

class Training(Page):
    form_model='player'
    form_fields = ['test_scenario']

    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey 


class TrainingNeighbor_1(Page):
    form_model='player'

    @staticmethod
    def get_form_fields(player:Player):  
        form_fields = ['dilemmatopic', 'majority', 'howmanyneighbors']
        random.shuffle(form_fields)  
        return form_fields
    
    @staticmethod
    def vars_for_template(player: Player):
        # Some made up responses of other players' to be displayed
        return dict(
            others_responses = [-1,1,1],
            scenario_title = "The Lost Cat",
            scenario_against = "Do not help with the search",
            scenario_neutral = "Verify the cat in the garden",
            scenario_for = "Help search near the sewage",
        )
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        wrong_question_keys = get_training_wrong_question_keys(player)
        player.participant.vars['training_wrong_question_keys'] = wrong_question_keys
        if wrong_question_keys:
            player.participant.vars['training_attempt'] -= 1
        else:
            player.participant.vars['training_success'] = True
        #print(f"Before next Training attempt: {player.participant.vars['training_attempt']}, {player.participant.vars['training_success']}")
    
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1 and player.participant.complete_presurvey 
        # return player.participant.training_attempt == 3 and not player.participant.training_success and player.round_number == 1 and not player.participant.failed_attention_check and player.participant.gives_consent


class TrainingNeighbor_2(Page):
    form_model='player'
    #form_fields = ['dilemmatopic', 'majority', 'howmanyneighbors']
    @staticmethod
    def get_form_fields(player:Player):  
        form_fields = ['dilemmatopic', 'majority', 'howmanyneighbors']
        random.shuffle(form_fields)  
        return form_fields
    
    @staticmethod
    def vars_for_template(player: Player):
        # Some made up responses of other players' to be displayed
        return dict(
            others_responses = [-1,1,1],
            scenario_title = "The Lost Cat",
            scenario_against = "Do not help with the search",
            scenario_neutral = "Verify the cat in the garden",
            scenario_for = "Help search near the sewage",
            wrong_question_keys = player.participant.vars.get('training_wrong_question_keys', []),
        )
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        wrong_question_keys = get_training_wrong_question_keys(player)
        player.participant.vars['training_wrong_question_keys'] = wrong_question_keys
        if wrong_question_keys:
            player.participant.vars['training_attempt'] -= 1
        else:
            player.participant.vars['training_success'] = True
        #print(f"Training attempt: {player.participant.vars['training_attempt']}, {player.participant.vars['training_success']}")

    @staticmethod
    def is_displayed(player):
        #print(f"Round:{player.round_number} and Training Counter: {player.participant.vars['training_attempt']}")
        if player.round_number == 1 and player.participant.complete_presurvey:
            return player.participant.training_attempt == 2 and not player.participant.training_success


class AttentionCheck(Page):
    form_model = 'player'
    form_fields = ['attention_check']
    timeout_seconds = 60 # Set a timeout for the attention check
    @staticmethod
    def before_next_page(player, timeout_happened):
        if timeout_happened:
            player.participant.failed_attention_check = True # initially False
            player.participant.complete_presurvey = False # initially True
        else:
            if player.attention_check != 2: # wrong answer
               player.participant.failed_attention_check = True 
               player.participant.complete_presurvey = False

    @staticmethod
    def is_displayed(player:Player):
        if player.round_number == 1 and player.participant.complete_presurvey:
            return player.participant.training_attempt == 1 and not player.participant.training_success
    

class TrainingNeighbor_3(Page):
    form_model='player'

    @staticmethod
    def get_form_fields(player:Player):  
        form_fields = ['dilemmatopic', 'majority', 'howmanyneighbors']
        random.shuffle(form_fields)  
        return form_fields
    
    @staticmethod
    def vars_for_template(player: Player):
        # Some made up responses of other players' to be displayed
        return dict(
            others_responses = [-1,1,1],
            scenario_title = "The Lost Cat",
            scenario_against = "Do not help with the search",
            scenario_neutral = "Verify the cat in the garden",
            scenario_for = "Help search near the sewage",
            wrong_question_keys = player.participant.vars.get('training_wrong_question_keys', []),
        )
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        wrong_question_keys = get_training_wrong_question_keys(player)
        player.participant.vars['training_wrong_question_keys'] = wrong_question_keys
        if wrong_question_keys:
            player.participant.vars['training_attempt'] -= 1
            player.participant.complete_presurvey = False
        else:
            player.participant.vars['training_success'] = True
            player.participant.complete_presurvey = True

    @staticmethod
    def is_displayed(player):
        if player.round_number == 1 and player.participant.complete_presurvey:
            return player.participant.training_attempt == 1 


class ExperimentInstruction(Page):
    @staticmethod
    def is_displayed(player:Player):
        return player.participant.complete_presurvey and player.round_number == 1

class Scenario(Page):
    form_model = 'player'
    form_fields = ['political_charge', 'emotional_charge', 'response']

    @staticmethod
    def vars_for_template(player: Player):
        # Access the scenario based on the session variable
        scenario = player.participant.vars['scenario_order'][0]
        
        return dict(
            scenario_text = scenario['Text'],
            scenario_title = scenario['Title'],
            scenario_code = scenario['code'],
            scenario_against = scenario['Against'],
            scenario_neutral = scenario['Neutral'],
            scenario_for = scenario['For'],
        )
    def before_next_page(player: Player, timeout_happened):
        # Get the current scenario code
        player.scenario_code = player.session.SCE
        
        # Store the response in the participant's vars 
        player.participant.vars['all_responses'][player.scenario_code] = player.response
        # Combine all participants' all_responses dictionaries into a session-level variable
        if 'combined_responses' not in player.session.vars:
            player.session.vars['combined_responses'] = {}
            # Add the current player's all_responses dictionary to the combined dictionary
        
        player.session.vars['combined_responses'][player.participant.code] = player.participant.vars['all_responses']
        
        # In the last round, check whether the player is eligible for the discussion
        if player.round_number == C.NUM_ROUNDS:
            
            # If all responses are neutral, set eligible_notneutral from True to False    
            player.participant.eligible_notneutral = not all(
                response == 0 for response in player.participant.vars['all_responses'].values()
            )

            player.participant.complete_presurvey = player.participant.eligible_notneutral
            print(f"Debug: eligibility: {player.participant.eligible_notneutral}" )
            """
            in the last round, check whether the player is eligible for the discussion
            if neutral in all options:
                ELIGIBLE = FALSE
            else: 
                ELIGIBLE = TRUE
            """

    @staticmethod
    def is_displayed(player:Player):
        return player.participant.complete_presurvey and player.round_number <= C.NUM_ROUNDS 


class Reminder(Page):
    form_model = 'player'

    @staticmethod
    def is_displayed(player: Player):
        return player.participant.complete_presurvey and player.round_number == C.NUM_ROUNDS
    

    
# # For testing manually (without bots) NOTE: don't forget to replace the page_sequence with the full sequence
# page_sequence = [Introduction, AudioCheck,
#                 Scenario, Commitment]

#Full page sequence
page_sequence = [Introduction, TimeSelection, AudioCheck, NeighborhoodInstruction, Scenario]
