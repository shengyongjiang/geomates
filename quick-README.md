# Quick README of Project

## Running our project

Other steps are the same, except for running the ACT-R agent is called by:
`sbcl --load "agent.lisp"`

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
├── geomates/                     # Main game folder
│   ├── act-r-experiment.lisp     # Interface between ACT-R and the game
│   ├── geomates.lisp             # Main game server implementation
│   ├── levels.lisp               # Game level definitions
│   ├── model-dummy.lisp          # Basic ACT-R agent model
│   ├── README.md                 # This documentation file
│   ├── viewer.html               # Web-based game viewer
│   └── etc.
│
├── code/                         # Agent code folder
│   ├── model-deepseek-agent1.lisp
│   └── navigation-functions.lisp
│
├── agent.lisp                    # Main entry point for running the ACT-R agent
│
└── Dockerfile                    # Docker configuration for containerized setup


navigation-functions.lisp
Functions that assist the ACT-R agent in determining how to navigate through the GeoMates environment. 
It includes functions for detecting platform gaps, finding the next action for both disc and rectangle agents to move toward diamonds, and converting movement commands between different formats (WASD strings to action symbols).

```

## Key Files

- **agent.lisp**: The main entry point that loads ACT-R, the experiment interface, navigation functions, and the agent model.
- **code/model-dummy.lisp**: The ACT-R agent model with platform detection and navigation capabilities.
- **code/navigation-functions.lisp**: Helper functions for agent navigation and decision-making.
