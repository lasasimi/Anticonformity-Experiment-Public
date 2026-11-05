# myapp/tests.py

from otree.api import Bot, SubmissionMustFail
from . import *

class PlayerBot(Bot):
    def play_round(self):
        yield Submission(Feedback, 
                         dict(feedback_final='This is a test feedback.'), 
                         check_html=False)
        yield Submission(MyPage, 
                    check_html=False)