# matching validation sampling

## Sampling validation datasets

TODO

## Exporting the sampling for annotation

### Installation

Tested with python3.12:
```shell
python -m venv export
source ./export/bin/activate
pip install -r requirements.txt
```
### Run
```shell
python export_to_validation.py -layer1 data/fr/strasbourg_building_2011.gpkg -layer2 data/fr/strasbourg_building_2021.gpkg -date1 2011 -date2 2021 -wmts1 ORTHOIMAGERY.ORTHOPHOTOS2011 -wmts2 ORTHOIMAGERY.ORTHOPHOTOS -select data/fr/strasbourg_sample.gpkg -select_layer strasbourg -radius 100.0 -output_directory VAL
```