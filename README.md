# GeoMates Setup Guide for macOS

**Important Note**

If you already setup geomates from <https://gitlab.isp.uni-luebeck.de/hai/geomates>, please check the <https://github.com/shengyongjiang/geomates/blob/master/quick-README.md> to run this project

This guide is a amendement readme.md for porject of <https://gitlab.isp.uni-luebeck.de/hai/geomates/-/blob/main/README.md?ref_type=heads> , it will help you set up and run the GeoMates environment on macOS.

## Prerequisites

- Docker Desktop for Mac
- Steel Bank Common Lisp (SBCL)
- Python 3
- Git
- the readme under the geomates folder

## Installation Steps

### 1. Install Required Software

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install SBCL and Python
brew install sbcl python

# Install QuickLisp (Common Lisp package manager)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
# In the SBCL prompt:
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

### 2. Clone the Repository

```bash
git clone https://gitlab.isp.uni-luebeck.de/hai/geomates
cd geomates
```

### 3. Build and Run the Docker Container

```bash
# Build the Docker image
docker-compose build

# Start the GeoMates server in the background
docker-compose up -d

# Start the GeoMates server in the background and show the logs
# !!! this is useful and used most of the time
docker-compose up -d && docker-compose logs -f

# NOTE:
# if we setup env at docker, then we don't need box2d
```

### 4. Start a Local Web Server

To view the game interface, start a local web server:

```bash
# 8081 is because 8000 is already used by my personal other project
python -m http.server 8081
```

You can now access the game interface at <http://localhost:8081/viewer.html>

### 5.  Download and Unzip ACT-R 7.x

```bash
# Download ACT-R 7.x
curl -L http://act-r.psy.cmu.edu/actr7.x/actr7.x.zip -o actr7.x.zip

# Unzip the file
unzip actr7.x.zip -d actr7.x
```

### 6. Run the ACT-R Agent

In a new terminal window:

```bash
# Navigate to your project directory
cd path/to/geomates

# if you find error message, you migh need clean complie file of listp
# find actr7.x -name "*.fasl" -delete
# Start SBCL with ACT-R and the GeoMates experiment

sbcl --load "entry.lisp"
# the above lisp contain the below diffrent lisp files
# sbcl --load "actr7.x/load-act-r.lisp" --load "geomates/act-r-experiment.lisp" --eval '(load-act-r-model "geomates/model-dummy.lisp")'
```

At the SBCL prompt, start the experiment:

To open the ACT-R environment GUI (if available):

```lisp
(run-environment)
```

To run the experiment:

```lisp
(geomates-experiment)
```

## Troubleshooting

### Docker Connection Issues

If you see "Broken pipe" errors when connecting from ACT-R to the Docker container:

1. Make sure the Docker container is running:

   ```bash
   docker-compose ps
   ```

2. Check the Docker logs:

   ```bash
   docker-compose logs
   ```

3. Restart the Docker container:

   ```bash
   docker-compose down
   docker-compose up -d
   ```

### ACT-R Connection Issues

If ACT-R cannot connect to the GeoMates server:

1. Ensure ports are properly exposed in docker-compose.yml:

   ```yaml
   ports:
     - "8000:8000"
     - "45678:45678"
   ```

2. Try using host network mode in docker-compose.yml:

   ```yaml
   network_mode: "host"
   ```

3. Check if the server is binding to the correct address in the GeoMates code.

### Docker Crashes

If Docker crashes when running `(geomates-experiment)`:

1. Run Docker in foreground mode to see errors:

   ```bash
   docker-compose down
   docker-compose up
   ```

2. Increase Docker resources in Docker Desktop settings.

## Directory Structure

```
project_root/
├── actr7.x/                  # ACT-R source files
│   └── load-act-r.lisp       # ACT-R loader
└── geomates/                 # GeoMates files
    ├── act-r-experiment.lisp # Experiment interface
    ├── model-dummy.lisp      # Example agent model
    └── viewer.html           # Web interface
```

```plain
┌─────────────────────────┐      ┌─────────────────────────┐
│     Local Machine       │      │    Docker Container     │
│                         │      │                         │
│  ┌─────────────────┐    │      │  ┌─────────────────┐    │
│  │     ACT-R       │    │      │  │    GeoMates     │    │
│  │  Client Command │    │      │  │  Server Command │    │
│  │                 │────┼──────┼──►                 │    │
│  └─────────────────┘    │      │  └─────────────────┘    │
│                         │      │                         │
└─────────────────────────┘      └─────────────────────────┘
        Port 45678 ◄─────────────────────► Port 45678
```

## Client-Server Architecture

- The Docker container runs the GeoMates server (environment)
- The ACT-R model runs on your local machine (agent)
- They communicate over TCP/IP on port 45678
- The web interface is available on port 8000 (Docker) or 8081 (local server)

## Usefull Tips

### the order of Luanch application oder is critical

1. Launch ACT-R  `sbcl --load "actr7.x/load-act-r.lisp" --load "geomates/act-r-experiment.lisp" --eval '(load-act-r-model "models/model-dummy.lisp")'`
2. Launch Game Server `docker-compose up -d && docker-compose logs -f`, if we open `http://localhost:8081/viewer.html`, we will see:
   - empty at game frame
   - in web pages' console messages(not browser console) display 'Received "undefined" from server.'
3. Swith Back to ACT-R lisp terminal,  run `(run-environment)` and/or `(geomates-experiment)`
4. In a terminal, `telnet localhost 45678`  (and only one telnet is needed to let model auto run)

### we could play game manually without lanuch ACT-R

1. Launch Game Server `docker-compose up -d && docker-compose logs -f`
2. In two different terminals, `telnet localhost 45678`  ,  2 agents  will connected, and click termial then press w/a/s/d to move the agent
