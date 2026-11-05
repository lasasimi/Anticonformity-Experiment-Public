# myapp/tests.py

from otree.api import Bot
from . import * 

class PlayerBot(Bot):
    def play_round(self):
        yield Introduction, dict(
            gives_consent=True)
        yield Demographics, dict(
            age=34,
            gender="Woman",
            education_lvl='Less than high school', 
            neighborhood_type='Urban')
        yield NeighborhoodInstruction
        # This is because the submit button is not a default submit button
        yield Submission(Training, 
                         {'test_scenario': '-1'}, 
                         check_html=False)    
        yield TrainingNeighbor_1, dict(
            dilemmatopic=1,
            majority=1,
            howmanyneighbors=1)

        yield ExperimentInstruction
        if self.player.id_in_group in range(1,5):
            yield Scenario, dict(
                political_charge=1,
                emotional_charge=1,
                response=-1
            )
        elif self.player.id_in_group in range(5, 9):
            yield Scenario, dict(
                political_charge=1,
                emotional_charge=1,
                response=1
            )
        elif self.player.id_in_group in range(9, 13):
            yield Scenario, dict(
                political_charge=1,
                emotional_charge=1,
                response=1
            )
        elif self.player.id_in_group in range(13, 17):
            yield Scenario, dict(
                political_charge=1,
                emotional_charge=1,
                response=-1
            )
        elif self.player.id_in_group in range(17, 21):
            yield Scenario, dict(
                political_charge=1,
                emotional_charge=1,
                response=1
            )
        yield Commitment, dict(
            commit_attention_Q1=True,
            commit_attention_Q2=True,
            commit_attention_Q3=True,
        )
