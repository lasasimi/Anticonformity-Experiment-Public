from otree.api import *
import pandas as pd # type: ignore
import numpy as np # type: ignore
import random 
import time

doc = """
Pre-survey for the Prolific Full Experiment.
This app collects demographic information and responses to a series of scenarios (only +1 For or -1 Against)
We will add a task training, and an attention check.
After pre-survey, participants will be redirected to the main experiment app (and we send the data from this app for the Grouping)
Whilst we group them, they will see a waiting page, ideally with a description of the task or a game to play.
"""

def open_CSV(filename):
    """
    :param filename: opinions.csv
    :return: a list of statements for the session
    """
    temp = pd.read_csv(filename, sep=',')
    return temp


class C(BaseConstants):
    NAME_IN_URL = 'presurvey'
    PLAYERS_PER_GROUP = None

    # REMEMBER TO CHANGE TO POLITICAL/NON-POLITICAL FRAMING DEPENDING ON THE EXPERIMENTAL DESIGN
    CSV = open_CSV('presurvey/scenarios_1np.csv')
    SCENARIOS = CSV.to_dict(orient='records')
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


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

    total_correct = models.IntegerField()
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
    form_fields = ['gives_consent']

    @staticmethod
    def is_displayed(player:Player):
        return player.round_number == 1 
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        player.participant.gives_consent = player.gives_consent
        player.participant.complete_presurvey = player.participant.gives_consent # Assigning active status based on consent
 
 
class AudioCheck(Page):
    form_model = 'player'
    form_fields = ['audio_answer', 'audio_answer_image', 'audio_unlocked']

    @staticmethod
    def before_next_page(player, timeout_happened):
        player.participant.audio_unlocked = player.audio_unlocked
        if player.audio_answer !=4 or player.audio_answer_image !=5: # incorrect answer
            player.participant.gives_consent = False
            player.participant.complete_presurvey = player.participant.gives_consent # Assigning active status based on consent

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


class Commitment(Page):
    form_model = 'player'

    @staticmethod
    def get_form_fields(player:Player):  
        form_fields = ['commit_attention_Q1','commit_attention_Q2', 'commit_attention_Q3']
        random.shuffle(form_fields)  
        return form_fields
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        total_commitment = np.sum([player.commit_attention_Q1, player.commit_attention_Q2,
                                   player.commit_attention_Q3])
        
        if total_commitment < 3:
            player.participant.failed_commitment = True # initially false
            player.participant.complete_presurvey = False
        else:
            player.participant.complete_presurvey = True # this is to check if participant moves forward to the mock app
            player.participant.wait_page_arrival = time.time()

        print(f"Commitment failed?: {player.participant.failed_commitment}, Complete presurvey?: {player.participant.complete_presurvey}")
    @staticmethod
    def is_displayed(player:Player):
        return player.participant.complete_presurvey and player.round_number == C.NUM_ROUNDS
        """
        only displayed if participant eligible
        if not eligible, plan accordingly. 
        """
# # For testing manually (without bots) NOTE: don't forget to replace the page_sequence with the full sequence
# page_sequence = [Introduction, AudioCheck,
#                 Scenario, Commitment]

#Full page sequence
page_sequence = [Introduction, AudioCheck, Demographics, NeighborhoodInstruction, Training, TrainingNeighbor_1, 
                 TrainingNeighbor_2, AttentionCheck, TrainingNeighbor_3, ExperimentInstruction,
                 Scenario, Commitment]
