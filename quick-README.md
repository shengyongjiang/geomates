# Quick README of Project

## Running our project

Other steps are the same, except for running the ACT-R agent is called by:
`sbcl --load "agent.lisp"`, and for a differnt agent2 `sbcl --load "agent2.lisp"`

(Replacement of
`sbcl --load "actr7.x/load-act-r.lisp" --load "geomates/act-r-experiment.lisp" --eval '(load-act-r-model "code/model-deepseek-agent1.lisp")'
`)

## Folder/files structure

This document is a quick amendment for `geomates/README.md`.
Assume ACT-R, box2d and geomates server are all set up, with the below folder structure:

```plain
/project_root
├── actr7.x/                      # ACT-R framework
│   ├── load-act-r.lisp           # Script to load ACT-R
│   └── etc.
│
├── geomates/ # Provided Game folder (no changes except levels.php)
│   ├── levels.lisp     # Test levels, Need to be put here and rebuild docker
│   └── etc.
│
├── code/                         # Agent code folder
│   │── levels.lisp  # Test levels(A simple copy from geomates/levels.lisp for project submision)
│   │     
│   ├── model-deepseek-agent1.lisp  # Agent1 model
│   ├── navigation-functions.lisp   # Agent1 function
│   │
|   └── model-deepseek-agent2.lisp  # Agent2 function
│
├── agent.lisp              # Entry point for running agent1
├── agent2.lisp             # Entry point for running agent2
|
├── quick-README.md         # if you have setup env, just want test our model quickly
├── README.md               # if you want chekc how to setup env and run this project from beging
│
└── docker-compose.yml       # Docker Compose configuration for containerized setup


navigation-functions.lisp
Functions that assist the ACT-R agent in determining how to navigate through the GeoMates environment. 

It includes functions for detecting platform gaps, finding the next action for both disc and rectangle agents to move toward diamonds, and converting movement commands between different formats (WASD strings to action symbols).

model-deepseek-agent1.lisp
The model file implements an ACT-R agent production that can identify its character type (disc or rectangle), locate objects in the environment, and execute a series of planned movements to collect diamonds. It uses a state-based approach with intention tracking and action queues to coordinate movement and try cooperation between agents.

levels.lisp
Different level from easy to hard
```

## Key Files

- **agent.lisp**: The main entry point that loads ACT-R, the experiment interface, navigation functions, and the agent model.
- **code/model-dummy.lisp**: The ACT-R agent model with platform detection and navigation capabilities.
- **code/navigation-functions.lisp**: Helper functions for agent navigation and decision-making.
