# myapp/tests.py

from otree.api import Bot, SubmissionMustFail
from . import *

class PlayerBot(Bot):
    def play_round(self):
        if ExitPage.is_displayed(self.player):
            yield ExitPage