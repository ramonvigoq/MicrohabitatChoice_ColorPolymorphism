# MicrohabitatChoice_ColorPolymorphism

Raw data files and analysis codes from the study '**Color-based microhabitat choice does not contribute to existing shell color polymorphisms in a marine snail**', by Ramón Vigo, Rubén Camesella, Carlos Pérez, Víctor Martínez-Mariño, André Vidal-Capón, Juan Gefaell, and Emilio Rolán-Alvarez, under review in _Journal of Molluscan Studies_
## Description of the data and file structure

We conducted two mark-recapture experiments to study the role of microhabitat choice in the maintenance of *Littorina saxatilis* shell color polymorphsim, particularly within intermediate populations of a color cline along de Ria de Vigo (NE Iberian Peninsula). 
We followed slightly different designs for each experiment, but with the same objective: determine in the field whether this species actively chooses a microhabitat and if this decision can be color-based (matching shell and substrate color).

## File and variables

### File: MicrohabitatChoice_RawData.xlsx
**Description:** 
This file is divided into 4 sheets, corresponding with the raw data and the codes of the variables for each experiment:
- 'HC_2023': Raw data of Experiment 1 (Canabal)
- 'CODES_2023': Codes of the variables included in Experiment 1 (Canabal)
- 'HC_2024': Raw data of Experiment 2 (Portino)
- 'CODES_2024': Codes of the variables included in Experiment 2 (Portino)
## Experiment 1
#### Variables description

| Variable            | Description                                                                 |
| ------------------- | --------------------------------------------------------------------------- |
| quadrat             | Quadrat replicate                                                           |
| habitat_in          | Microhabitat where the snail was released                                   |
| q_distance_mean     | Mean distance from the initial point to the next microhabitat in cm         |
| q_distance_variance | Mean distance from the initial point to the next microhabitat in cm         |
| area                | Microhabitat area of each treatment in cm                                   |
| snail_ID            | Individual snail ID                                                         |
| color               | Shell color morph                                                           |
| size                | Shell length in mm                                                          |
| scar                | Scar presence                                                               |
| recapture           | Whether the snail was recaptured                                            |
| habitat_out         | Microhabitat where the snail was recaptured                                 |
| change              | Whether the snail changed its habitat or remained                           |
| direction           | Overall direction of the recaptured snail movement                          |
| snail_distance      | Distance from the initial point to the point where the snail was recaptured |
| sex                 | Sex of the snail, based on the presence or absence of a penis               |

##### Qualitative variables levels

| Levels | habitat_in         | color    | scar  | recapture | habitat_out        | direction | sex      |
| -----: | ------------------ | :------- | ----- | --------- | ------------------ | --------- | -------- |
|     -1 |                    |          |       |           |                    | earth     |          |
|      0 | bare rock          | aurantia | true  | true      | bare rock          |           | immature |
|      1 | black lichen patch | lineata  | false | false     | black lichen patch | sea       | male     |
|      2 |                    |          |       |           |                    |           | female   |


## Experiment 2
#### Variables description

| Variable       | Description                                                                 |
| -------------- | --------------------------------------------------------------------------- |
| session        | Temporal session of the experiment                                          |
| quadrat        | Quadrat replicate                                                           |
| snail_ID       | Individual snail ID                                                         |
| color          | Shell color morph                                                           |
| size           | Shell length in mm                                                          |
| scar           | Scar presence                                                               |
| recapture      | Whether the snail was recaptured                                            |
| habitat        | Microhabitat where the snail was recaptured                                 |
| inside         | Whether the snail was found inside or outside the quadrat                   |
| slope          | Whether the snail was found below or above its initial point                |
| direction      | Overall direction of the recaptured snail movement                          |
| snail_distance | Distance from the initial point to the point where the snail was recaptured |
| sex            | Sex of the snail, based on the presence or absence of a penis               |

##### Qualitative variables levels

| Levels | color    | scar  | recapture | habitat            | inside | direction    |
| -----: | :------- | ----- | --------- | ------------------ | ------ | ------------ |
|     -1 |          |       |           |                    |        | earth        |
|      0 | aurantia | true  | true      | bare rock          | false  | intermediate |
|      1 | lineata  | false | false     | barnacle-covered   | true   | sea          |
|      2 |          |       |           | crevice            |        |              |
|      3 |          |       |           | black lichen patch |        |              |
|      4 |          |       |           | tide pool          |        |              |

