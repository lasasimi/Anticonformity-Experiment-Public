from otree.api import *


doc = """
Your app description
"""


class C(BaseConstants):
    NAME_IN_URL = 'Pay_phase1'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    pass


class Reminder(Page):
    form_model = 'player'

    @staticmethod
    def vars_for_template(player: Player):
        selected_slots = player.participant.vars.get('selected_time_slots_display', [])
        return dict(selected_time_slots=selected_slots)
    
    @staticmethod
    def is_displayed(player: Player):
        # Hide reminder for screened-out participants, failed commitment, or those who did not commit to phase 2 (selected 99)
        if (not player.participant.eligible_notneutral
            or player.participant.failed_commitment
            or not player.participant.commit_phase2 
            or not player.participant.gives_consent):
            return False
        else:
            return not player.participant.failed_attention_check and player.participant.active
        

    @staticmethod
    def js_vars(player: Player):
       if player.participant.complete_presurvey:
           return dict(pay=player.subsession.session.config['eligiblelink'])
       elif not player.participant.complete_presurvey:
           return dict(pay=player.subsession.session.config['noteligiblelink'])
       
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
                pay=player.subsession.session.config['noteligiblelink']
            )
        elif player.participant.eligible_notneutral:
            player.participant.reason="We are looking forward to your participation in the Phase 2. You will receive your payment for finishing this part of the study. Thanks for your time!"
            return dict(
                pay=player.subsession.session.config['eligiblelink']
            )
        
    @staticmethod
    def is_displayed(player: Player):
        # Participants who did not commit to phase 2 should skip this app and continue to noPay
        if not player.participant.commit_phase2 or not player.participant.gives_consent:
            return False
        # if they were not eligible, we need to check if they were active and did not fail the attention check
        if not player.participant.eligible_notneutral: # those who were not eligible
            return player.participant.active and not player.participant.failed_attention_check 
        else:
            return not player.participant.failed_attention_check and player.participant.active
        
page_sequence = [Reminder, MyPage]
