from otree.api import *


doc = """
Your app description
"""


class C(BaseConstants):
    NAME_IN_URL = 'noPay'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    feedback_final = models.LongStringField(label="Please provide your feedback here:",
                                            blank=True)


# PAGES
class ExitPage(Page):
    # For not consenting participants
    form_model = 'player'
    form_fields = ['feedback_final']

    # Conditional JS variables based on participant status, same links but different reasons
    @staticmethod
    def js_vars(player: Player):
        # From presurvey app
        if player.participant.training_attempt == 0:
            player.participant.reason="you did not pass the Training phase by answering incorrectly for too many times."
        elif player.participant.failed_attention_check:
            player.participant.reason="you did not pass the attention check."
        elif not player.participant.gives_consent:
            player.participant.reason="you did not consent to participate or could not enable the audio output."
        if 'commit_phase2' in player.participant.vars:
            if not player.participant.commit_phase2:
                player.participant.reason="you did not commit to participate in the second phase of the experiment."
        # From mock app
        elif not player.participant.active:
            player.participant.reason="you have been timed out for inactivity."
        elif player.participant.away_long:
            player.participant.reason="you were away for too long during the the wait."
        return dict(
                nopay=player.subsession.session.config['returnlink'],
            )

    @staticmethod
    def is_displayed(player: Player):
        # if they did not complete presurvey, they did not get payment either because they were not active in the mock app, or did not pass the attention/training(complete_presurvey), or did not give consent
        if not player.participant.complete_presurvey:
            if 'commit_phase2' in player.participant.vars:
                return not player.participant.gives_consent or not player.participant.commit_phase2 # show noPay if did not commit to phase
            else:
                return not player.participant.active or not player.participant.gives_consent or player.participant.failed_attention_check or player.participant.training_attempt == 0 
        else:
            return not player.participant.active or player.participant.away_long
page_sequence = [ExitPage]
