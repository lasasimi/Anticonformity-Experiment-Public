# myapp/tests.py

from otree.api import Bot, SubmissionMustFail
from . import *

class PlayerBot(Bot):
    def play_round(self):
        if Feedback.is_displayed(self.player):
            yield Submission(Feedback, 
                            dict(feedback_final='This is a test feedback.',
                                 future_participation=True), 
                            check_html=False)
        if MyPage.is_displayed(self.player):
            yield Submission(MyPage, 
                        check_html=False)