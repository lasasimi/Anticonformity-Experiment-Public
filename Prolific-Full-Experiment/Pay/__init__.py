from otree.api import *


doc = """
Your app description
"""


class C(BaseConstants):
    NAME_IN_URL = 'Pay'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    feedback_final = models.LongStringField(label="Please provide your feedback here:",
                                            blank=True)
    future_participation = models.BooleanField(
        label="I would like to participate in the next part of this study and receive an invitation via Prolific.",
        choices=[[True, 'Yes'], [False, 'No']],
        blank=False,
        initial=None,
        widget=widgets.RadioSelect,)


class Feedback(Page):
    form_model = 'player'
    form_fields = ['feedback_final', 'future_participation']
    
    @staticmethod
    def is_displayed(player: Player):
        # if they were not eligible, we need to check if they were active and did not fail the attention check
        if not player.participant.eligible_notneutral or player.participant.failed_commitment: # those who were not eligible or failed commitment are not shown this page
            return False
        else:
            return not player.participant.failed_attention_check and (player.participant.single_group or player.participant.active)
        

    @staticmethod
    def js_vars(player: Player):
       if player.participant.complete_presurvey and not player.participant.single_group:
           return dict(pay=player.subsession.session.config['maxbonuslink'])
       elif player.participant.complete_presurvey and player.participant.single_group:
           return dict(pay=player.subsession.session.config['waitingbonuslink'])
       elif not player.participant.complete_presurvey:
           return dict(pay=player.subsession.session.config['screenedoutlink'])
       
# PAGES
class MyPage(Page):
    form_model = 'player'
    # JS vars to determine which link and reason to display based on if they finished the entire app (participant.single_group == False) or waited too long
    @staticmethod
    def js_vars(player: Player):
        # completed the presurvey, but were not eligible (answered neutral in all questions)
        if not player.participant.eligible_notneutral:
            player.participant.reason="You answered the middle option in the previous question. We are looking for participants who have strong opinions about the issue, so you were screened out. You will still receive your base payment. Thanks for your time!"
            return dict(
                pay=player.subsession.session.config['screenedoutlink']
            )
        # completed the presurvey, but did not commit
        elif player.participant.failed_commitment == True:
            player.participant.reason="You did not commit to entering the next phase of the study. You will still receive your base payment. Thanks for your time!"
            return dict(
                pay=player.subsession.session.config['screenedoutlink']
            )
        # completed the presurvey, but did not complete the mock app
        elif player.participant.single_group == True:
            player.participant.reason="You were the only participant in your group and we could not find you other participants to join your group. You will receive your payment for waiting. Thanks for your time!"
            return dict(
                pay=player.subsession.session.config['waitingbonuslink']
            )
        # completed the mock app
        elif player.participant.single_group == False:
            player.participant.reason="Thanks for participating! Since you completed the entire study, you will receive the bonus payment."
            return dict(
                pay=player.subsession.session.config['maxbonuslink']
            )
        
    @staticmethod
    def is_displayed(player: Player):
        # if they were not eligible, we need to check if they were active and did not fail the attention check
        if not player.participant.eligible_notneutral: # those who were not eligible
            return player.participant.active and not player.participant.failed_attention_check 
        else:
            return not player.participant.failed_attention_check and (player.participant.single_group or player.participant.active)
        
page_sequence = [Feedback, MyPage]
