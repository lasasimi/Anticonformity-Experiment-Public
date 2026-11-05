# myapp/tests.py

from otree.api import Bot, SubmissionMustFail
from . import *

class PlayerBot(Bot):
    def play_round(self):
        if self.player.round_number == 1:
            yield Submission(Nudge,
                             dict(nudge_training=random.choice([-1, 0, 1])),
                             check_html=False)
            yield Submission(NudgeTraining, 
                             dict(nudge_training_two=random.choice([-1, 0, 1])),
                             check_html=False)
            yield Submission(NudgeTrainingLast, 
                             dict(nudge_training_three=random.choice([-1, 0, 1])),
                             check_html=False)
            yield Phase3

            yield Submission(Discussion, 
                        dict(new_response= random.choice([-1, 0, 1])),
                        check_html=False)
        elif self.player.round_number < C.NUM_ROUNDS:
            yield Submission(Discussion, 
                        dict(new_response= random.choice([-1, 0, 1])),
                        check_html=False)
        else:
            yield Submission(Discussion, 
                        dict(new_response= random.choice([-1, 0, 1])),
                        check_html=False)
            yield Submission(Additional, dict(aff_pol_A=10, 
                                   aff_pol_F=10,))
            yield FinalRound
        # if self.player.round_number == 1:
        #     yield Discussion, dict(
        #         new_response = -1)
        # elif self.player.round_number == 2:
        #     yield Discussion, dict(
        #         new_response = 0)
        # else:
        #     yield Discussion, dict(
        #         new_response = 1)
            