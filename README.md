# examples.train

Educational scala project based on the topic of railway schedules, simulation and optimization.

## Challenges

Intended to be used for challenges:

| Challenge | Goal | Score |
|---|---|---|
| Network coverage (single train) | Cover some randomly generated connected network. | All stations are on the trains's route. |
| Network coverage (mono speed) | Cover some randomly generated connected network with a random number of trains (with equal speed). | Maximum length of all routes. |
| Network coverage (partitioned) | Cover some randomly generated partitioned network. | Maximum length of all routes. |
| Simulated schedule (multi speed) | Cover some randomly generated partitioned network with a random number of trains (with different speed). | Time on which the last of all stations is reached by a train. Alternatively sum of train operation times. |
| Passenger transport | Transport most passengers. | Sum of (passenger times travelled time - waiting time). |
| Continuous schedule | Transport most passengers over elapsing time. | Same as passenger transport whereas the score is divided by elapsed time. |

| Challenge | Scorer |
|---|---|
| Basic | CollisionDetector |
| Network coverage | NetworkAnalyser |
| Simulated schedule | SimulationAnalyser |
| Passenger transport | PassengerAnalyser |
| Continuous transport | ContinuousAnalyser |
