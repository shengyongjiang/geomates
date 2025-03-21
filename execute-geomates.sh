#!/bin/bash
# Filename: execute-geomates-window.sh
# Description: Execute commands in a new window within existing geomates-1 session
# Usage: ./execute-geomates-window.sh [kill]

# Target tmux session and window
SESSION="geomates-1"
WINDOW_NAME="geomates-work"
WINDOW_INDEX=1  # The new window will be created as window 1
WORKING_DIR="~/lubeck-modules/31_Intelligent_Agents/DeepSeek_ICA_Agent"

# Kill window function
kill_window() {
    if tmux has-session -t "$SESSION" 2>/dev/null; then
        if tmux list-windows -t "$SESSION" | grep -q "$WINDOW_INDEX"; then
            echo "Killing window $WINDOW_INDEX in session $SESSION"
            tmux kill-window -t "$SESSION:$WINDOW_INDEX"
        else
            echo "Window $WINDOW_INDEX doesn't exist in session $SESSION"
        fi
    else
        echo "Session $SESSION doesn't exist"
    fi
}

# Kill window if requested
if [ "$1" = "kill" ]; then
    kill_window
    exit 0
fi

# Check if the session exists, if not create it
if ! tmux has-session -t "$SESSION" 2>/dev/null; then
    echo "Creating new session: $SESSION"
    tmux new-session -d -s "$SESSION" -c "$WORKING_DIR"
    echo "Session $SESSION created"
fi

# Kill the window if it exists and create a new one
if tmux list-windows -t "$SESSION" | grep -q "$WINDOW_INDEX"; then
    echo "Killing existing window $WINDOW_INDEX to start fresh"
    tmux kill-window -t "$SESSION:$WINDOW_INDEX"
fi

# Create a new window
echo "Creating new window in session $SESSION"
tmux new-window -t "$SESSION:$WINDOW_INDEX" -n "$WINDOW_NAME" -c "$WORKING_DIR"

# Allow time for window creation
sleep 0.5

# Create a simple layout with just the basic commands
# Start with a single pane (pane 0)
# Split horizontally to create pane 0 (top) and pane 3 (bottom)
tmux split-window -v -t "$SESSION:$WINDOW_INDEX.0" -c "$WORKING_DIR"

# Split pane 0 vertically to create pane 0 (left) and pane 1 (right)
tmux split-window -h -t "$SESSION:$WINDOW_INDEX.0" -c "$WORKING_DIR"

# Split pane 1 vertically to create pane 1 (left) and pane 2 (right)
tmux split-window -h -t "$SESSION:$WINDOW_INDEX.1" -c "$WORKING_DIR"

# Allow layout to settle
sleep 0.5

# Resize the top panes to make the first row smaller
tmux resize-pane -t "$SESSION:$WINDOW_INDEX.0" -y 10
tmux resize-pane -t "$SESSION:$WINDOW_INDEX.1" -y 10
tmux resize-pane -t "$SESSION:$WINDOW_INDEX.2" -y 10

# Function to send commands to a specific pane
send_command() {
    local pane=$1
    local command=$2
    
    echo "Sending command to pane $pane: $command"
    tmux send-keys -t "$SESSION:$WINDOW_INDEX.$pane" "cd $WORKING_DIR && $command" C-m
}

# Execute commands in respective panes
# send_command 0 "docker-compose up -d && docker-compose logs -f"
# send_command 0 "docker-compose down && docker-compose up -d && docker-compose logs -f"
# send_command 0 "docker exec -it deepseek_ica_agent_geomates_1 bash -c 'sbcl --script geomates.lisp'"
# send_command 0 "docker exec -it deepseek_ica_agent_geomates_1  sbcl --script geomates.lisp"
send_command 0 "docker-compose restart && docker exec -it deepseek_ica_agent_geomates_1  sbcl --script geomates.lisp"
send_command 1 "sleep 0.1 && open -a \"Google Chrome\" \"http://localhost:8081/viewer.html\""
send_command 2 "sleep 1 && telnet localhost 45678"

# Original command for reference
# send_command 3 "cd $WORKING_DIR && sbcl --load \"actr7.x/load-act-r.lisp\" --load \"geomates/act-r-experiment.lisp\"  --load \"models/navigation-functions.lisp\"   --eval '(load-act-r-model \""models/model-dummy.lisp"\")' --eval '(progn (sleep 1) (run 60) (run-environment))'"
send_command 3 "cd $WORKING_DIR && sbcl --load \"entry.lisp\""

# Select pane 3 (second row) to make it active
tmux select-pane -t "$SESSION:$WINDOW_INDEX.3"

# Attach to the session if not already attached
if [ -z "$TMUX" ]; then
    tmux attach-session -t "$SESSION:$WINDOW_INDEX"
else
    # If already in tmux but in a different session
    if [ "$(tmux display-message -p '#S')" != "$SESSION" ]; then
        echo "You are in a different session. Switch with: tmux switch-client -t $SESSION:$WINDOW_INDEX"
    # If already in the correct session but different window
    elif [ "$(tmux display-message -p '#I')" != "$WINDOW_INDEX" ]; then
        echo "You are in the correct session but different window."
        echo "Switch with: tmux select-window -t $WINDOW_INDEX"
        # Optionally automatically switch window:
        # tmux select-window -t $WINDOW_INDEX
    else
        echo "You are already in the correct window of the correct session."
    fi
fi