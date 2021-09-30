# gleason_extraction

This repository contains the programme developed in the peer-reviewed study "Accurate pattern-based extraction of complex Gleason score expressions from pathology reports" (https://doi.org/10.1016/j.jbi.2021.103850).

The study was conducted at the Finnish Cancer Registry (https://cancerregistry.fi/).

The code has been cleaned and documented after completion of the study without affecting any functionaly. 
To browse the repository in the state it was in at the completion of the study, go to
https://github.com/WetRobot/gleason_extraction/tree/7946cc2ae5403937a4b5ac30e6a74166af6b7507.

Contents:

- `example.R`: run this to see how it all works.
- `x_confusion_funs.R`: contains functions which define confusion statistics (sensitivity, etc.)
- `x_gleason_extraction_funs.R`: the regular expressions and how they are used are defined here.
- `x_pattern_extraction_funs.R`: more general pattern extraction functions.
- `x_util_funs.R`: general utility functions.


