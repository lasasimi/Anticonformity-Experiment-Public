from os import environ

SESSION_CONFIGS = [
    dict(
        name='all_N08_N04',
        app_sequence=['presurvey', 'mock', 'noPay', 'Pay'],
        num_demo_participants=8, # N08 N04 (must be a multiple of 4)
        display_name="all_N08_N04",
        # no consent, failed training, faield attention check, and became inactive in mock app
        returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
        # screened out, either because answered neutral (not eligible) or not wanting to commit
        screenedoutlink='https://app.prolific.com/submissions/complete?cc=CSD7X9S6',
        # finished presurvey, waited too long, not matched with other participants
        waitingbonuslink='https://app.prolific.com/submissions/complete?cc=CMKI4JO9', # base pay only
        # finished presurvey, commitment, AND completed mock app
        maxbonuslink=' https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', # base pay + waiting bonus OR max pay
        
        # maximum groups in each condition
        MAX_N04_p00 = 0,
        MAX_N08_p00 = 5,
        MAX_N04_p50 = 0,
        MAX_N08_p50 = 0,
        MAX_N04_p99 = 20,
        MAX_N08_p99 = 0,
        MAX_N04_p100 = 0,
        MAX_N08_p100 = 9,
        SCE = 's2_p', # scenario framing (s2_n or s2_p)
    ),
    # dict(
    #     name='all_old',
    #     app_sequence=['presurvey', 'mock_old', 'noPay', 'Pay'],
    #     num_demo_participants=100, # N08 N04 (must be a multiple of 4)
    #     display_name="all_old",
    #     # no consent, failed training, faield attention check, and became inactive in mock app
    #     returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
    #     # screened out, either because answered neutral (not eligible) or not wanting to commit
    #     screenedoutlink='https://app.prolific.com/submissions/complete?cc=CSD7X9S6',
    #     # finished presurvey, waited too long, not matched with other participants
    #     waitingbonuslink='https://app.prolific.com/submissions/complete?cc=CMKI4JO9', # base pay only
    #     # finished presurvey, commitment, AND completed mock app
    #     maxbonuslink=' https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', # base pay + waiting bonus OR max pay
        
    #     # maximum groups in each condition
    #     MAX_N04_p00 = 5,
    #     MAX_N08_p00 = 5,
    #     MAX_N04_p50 = 5,
    #     MAX_N08_p50 = 5,
    #     MAX_N04_p99 = 0,
    #     MAX_N08_p99 = 0,
    #     MAX_N04_p100 = 0,
    #     MAX_N08_p100 = 0,
    #     SCE = 's2_p', # scenario framing (s2_n or s2_p)
    # ),
    dict(
        name='N08_only',
        app_sequence=['presurvey', 'mock_N08_only', 'noPay', 'Pay'],
        num_demo_participants=20, # N08 N04 (must be a multiple of 4)
        display_name="N08_only",
        # no consent, failed training, faield attention check, and became inactive in mock app
        returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
        # screened out, either because answered neutral (not eligible) or not wanting to commit
        screenedoutlink='https://app.prolific.com/submissions/complete?cc=CSD7X9S6',
        # finished presurvey, waited too long, not matched with other participants
        waitingbonuslink='https://app.prolific.com/submissions/complete?cc=CMKI4JO9', # base pay only
        # finished presurvey, commitment, AND completed mock app
        maxbonuslink=' https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', # base pay + waiting bonus OR max pay
        
        # maximum groups in each condition
        MAX_N04_p00 = 5,
        MAX_N08_p00 = 5,
        MAX_N04_p50 = 5,
        MAX_N08_p50 = 5,
        MAX_N04_p99 = 0,
        MAX_N08_p99 = 0,
        MAX_N04_p100 = 0,
        MAX_N08_p100 = 0,
        SCE = 's2_p', # scenario framing (s2_n or s2_p)
    ),
    dict(
        name='N04_AandF',
        app_sequence=['presurvey', 'mock_N04_AandF', 'noPay', 'Pay'],
        num_demo_participants=20, # N08 N04 (must be a multiple of 4)
        display_name="N04_AandF",
        # no consent, failed training, faield attention check, and became inactive in mock app
        returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
        # screened out, either because answered neutral (not eligible) or not wanting to commit
        screenedoutlink='https://app.prolific.com/submissions/complete?cc=CSD7X9S6',
        # finished presurvey, waited too long, not matched with other participants
        waitingbonuslink='https://app.prolific.com/submissions/complete?cc=CMKI4JO9', # base pay only
        # finished presurvey, commitment, AND completed mock app
        maxbonuslink=' https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', # base pay + waiting bonus OR max pay
        
        # maximum groups in each condition
        MAX_N04_p00 = 5,
        MAX_N08_p00 = 5,
        MAX_N04_p50 = 5,
        MAX_N08_p50 = 5,
        MAX_N04_p99 = 0,
        MAX_N08_p99 = 0,
        MAX_N04_p100 = 0,
        MAX_N08_p100 = 0,
        SCE = 's2_p', # scenario framing (s2_n or s2_p)
    ),
    dict(
        name='N08_N04_Aonly',
        app_sequence=['presurvey', 'mock_N08_N04_Aonly', 'noPay', 'Pay'],
        num_demo_participants=100, # N08 N04 (must be a multiple of 4)
        display_name="N08_N04_Aonly",
        # no consent, failed training, faield attention check, and became inactive in mock app
        returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
        # screened out, either because answered neutral (not eligible) or not wanting to commit
        screenedoutlink='https://app.prolific.com/submissions/complete?cc=CSD7X9S6',
        # finished presurvey, waited too long, not matched with other participants
        waitingbonuslink='https://app.prolific.com/submissions/complete?cc=CMKI4JO9', # base pay only
        # finished presurvey, commitment, AND completed mock app
        maxbonuslink=' https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', # base pay + waiting bonus OR max pay
        
        # maximum groups in each condition
        MAX_N04_p00 = 1,
        MAX_N08_p00 = 0,

        MAX_N04_p50 = 3,
        MAX_N08_p50 = 1,

        MAX_N04_p100 = 0,
        MAX_N08_p100 = 0,

        MAX_N04_p99 = 0,
        MAX_N08_p99 = 0,
        SCE = 's2_p', # scenario framing (s2_n or s2_p)
    ),
    #     dict(
    #     name='N08_N04_Aonly_old',
    #     app_sequence=['presurvey', 'mock_N08_N04_Aonly_old', 'noPay', 'Pay'],
    #     num_demo_participants=40, # N08 N04 (must be a multiple of 4)
    #     display_name="N08_N04_Aonly_old",
    #     # no consent, failed training, faield attention check, and became inactive in mock app
    #     returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
    #     # screened out, either because answered neutral (not eligible) or not wanting to commit
    #     screenedoutlink='https://app.prolific.com/submissions/complete?cc=CSD7X9S6',
    #     # finished presurvey, waited too long, not matched with other participants
    #     waitingbonuslink='https://app.prolific.com/submissions/complete?cc=CMKI4JO9', # base pay only
    #     # finished presurvey, commitment, AND completed mock app
    #     maxbonuslink=' https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', # base pay + waiting bonus OR max pay
        
    #     # maximum groups in each condition
    #     MAX_N04_p00 = 6,
    #     MAX_N08_p00 = 9,
    #     MAX_N04_p50 = 5,
    #     MAX_N08_p50 = 8,
    #     MAX_N04_p99 = 0,
    #     MAX_N08_p99 = 0,
    #     MAX_N04_p100 = 0,
    #     MAX_N08_p100 = 0,
    #     SCE = 's2_p', # scenario framing (s2_n or s2_p)
    # ),
    dict(
        name='N04_Aonly',
        app_sequence=['presurvey_N04_Aonly', 'mock_N04_Aonly', 'noPay', 'Pay_N04_Aonly'], # different explanation on Pay app
        num_demo_participants=20, # N08 N04 (must be a multiple of 4)
        display_name="N04_Aonly",
        # no consent, failed training, faield attention check, and became inactive in mock app
        returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
        # screened out, either because answered neutral (not eligible) or not wanting to commit
        screenedoutlink='https://app.prolific.com/submissions/complete?cc=CSD7X9S6',
        # finished presurvey, waited too long, not matched with other participants
        waitingbonuslink='https://app.prolific.com/submissions/complete?cc=CMKI4JO9', # base pay only
        # finished presurvey, commitment, AND completed mock app
        maxbonuslink=' https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', # base pay + waiting bonus OR max pay
        
        # maximum groups in each condition
        MAX_N04_p00 = 5,
        MAX_N08_p00 = 5,
        MAX_N04_p50 = 5,
        MAX_N08_p50 = 5,
        MAX_N04_p99 = 0,
        MAX_N08_p99 = 0,
        MAX_N04_p100 = 0,
        MAX_N08_p100 = 0,
        SCE = 's2_p', # scenario framing (s2_n or s2_p)
    ),
    dict(
        name='Phase_1',
        app_sequence=['presurvey_part1b', 'Pay_phase1', 'noPay'], # different explanation and end links on Pay app (so we can group participants differently based on neutra/not neutral in the Prolific group)
        num_demo_participants=20, # N08 N04 (must be a multiple of 4),
        display_name='Phase_1',
        # no consent, failed training, faield attention check, and became inactive in mock app
        returnlink='https://app.prolific.com/submissions/complete?cc=CUN28996', # no pay, no bonus
        # noteliegible link (paid for completion but neutral)
        noteligiblelink='https://app.prolific.com/submissions/complete?cc=C10VZUJS',
        # eligibile link (paid for completion and non-neutral)
        eligiblelink='https://app.prolific.com/submissions/complete?cc=C1AKEBMQ', 
    
        SCE = 's2_p', # scenario framing (s2_n or s2_p)
        )
]


# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=1.00, participation_fee=0.00, doc=""
)

PARTICIPANT_FIELDS = ['audio_unlocked',
                      'gives_consent', 'training_attempt', 'training_success', 'no_consent', 'failed_commitment',
                      'treatment', 'scenario_order', 'all_responses', 'wait_page_arrival', 'failed_attention_check', 
                      'active', 'single_group', 'reason','player_ids', 'group_size', 'is_group_single',
                      'scenario','anticonformist','position','own_faction','other_faction','discussion_grp','complete_presurvey',
                      'eligible_notneutral', 'forced_response_counter', 'away_long', 'positive',
                      'nudge_training', 'correct_nudge_training', 'nudge_training_two','nudge_training_three', 'last_active', 'forced_response_remaining', 'control', 'too_many_forced', 'commit_phase2']
SESSION_FIELDS = ['combined_responses','scenario_counts', 
                  'N04_p00','N04_p100','N04_p50','N08_p00' ,'N08_p100','N08_p50', 'N08_p99','N04_p99',
                  'MAX_N04_p00','MAX_N04_p100','MAX_N04_p50','MAX_N08_p00','MAX_N08_p100','MAX_N08_p50','MAX_N08_p99', 'MAX_N04_p99',
                  'SCE', 'start_time']

ROOMS = [
    dict(name='phase2_p_20260526', display_name='phase2_p_20260526'),
    dict(name='phase2_p_20260527', display_name='phase2_p_20260527'),
]

GBAT_INACTIVE_SECONDS_UNTIL_PROMPT = 2 * 60
GBAT_INACTIVE_SECONDS_TO_CONFIRM = 15

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """ """

SECRET_KEY = '5749958540088'
