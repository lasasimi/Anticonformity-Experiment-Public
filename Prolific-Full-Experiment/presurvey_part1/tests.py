# myapp/tests.py

from otree.api import Bot
from . import * 

class PlayerBot(Bot):
    def play_round(self):
        # 1) Introduction / consent
        if Introduction.is_displayed(self.player):
            yield Introduction, dict(gives_consent=True)
            
        
        # 2) AudioCheck
        if AudioCheck.is_displayed(self.player):
            # IMPORTANT: these keys MUST match AudioCheck.form_fields
            yield Submission(
                AudioCheck,
                dict(
                    audio_unlocked=True,
                    audio_answer=4,        # correct option index/value
                    audio_answer_image=5,  # correct option index/value
                ),
                check_html=False,  # ignore JS/audio-specific HTML checks
            )
        if Demographics.is_displayed(self.player):
            yield Demographics, dict(
            age=34,
            gender="Woman",
            education_lvl='Less than high school', 
            neighborhood_type='Urban',
            political_affiliation=1)
        
        if NeighborhoodInstruction.is_displayed(self.player):
            yield NeighborhoodInstruction
        # This is because the submit button is not a default submit button
        if Training.is_displayed(self.player):
            yield Submission(Training, 
                            {'test_scenario': '-1'}, 
                            check_html=False)    
        if TrainingNeighbor_1.is_displayed(self.player):
            yield TrainingNeighbor_1, dict(
                dilemmatopic=1,
                majority=1,
                howmanyneighbors=1)

        if ExperimentInstruction.is_displayed(self.player):
            yield ExperimentInstruction

        if Scenario.is_displayed(self.player):
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
                    response=-1
                )
            elif self.player.id_in_group in range(13, 17):
                yield Scenario, dict(
                    political_charge=1,
                    emotional_charge=1,
                    response=1
                )
            elif self.player.id_in_group in range(17, 21):
                yield Scenario, dict(
                    political_charge=1,
                    emotional_charge=1,
                    response=-1
                )
            elif self.player.id_in_group in range(21, 25):
                yield Scenario, dict(
                    political_charge=1,
                    emotional_charge=1,
                    response=1
                )
            elif self.player.id_in_group in range(25, 29):
                yield Scenario, dict(
                    political_charge=1,
                    emotional_charge=1,
                    response=-1
                )
            elif self.player.id_in_group in range(29, 33):
                yield Scenario, dict(
                    political_charge=1,
                    emotional_charge=1,
                    response=1
                )
            elif self.player.id_in_group in range(33, 37):
                yield Scenario, dict(
                    political_charge=1,
                    emotional_charge=1,
                    response=-1
                )
            elif self.player.id_in_group in range(37, 41):
                yield Scenario, dict(
                    political_charge=1,
                    emotional_charge=1,
                    response=1
                )
        
        if Commitment.is_displayed(self.player):
            yield Commitment, dict(
                commit_attention_Q1=True,
                commit_attention_Q2=True,
                commit_attention_Q3=True,
            )
