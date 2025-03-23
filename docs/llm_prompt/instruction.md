# GeoMates Project Implementation Guide

## Project Overview

GeoMates is a cooperative physical simulation game where agents need to collect diamonds in a 2D environment. The game features two agents:

- A disc (yellow) that can jump
- A block/rectangle (red) that can change its width-to-height ratio

The goal is to collect as many diamonds as possible, with scoring as follows:

- 2 points for each diamond your agent collects
- 1 point for each diamond collected by the other agent

Some diamonds can be collected by individual agents, while others require cooperation.

## Game Mechanics

Based on the viewer.html file, we can understand the game environment:

1. **Game Elements**:
   - Rectangle agent (red): Can change its width-to-height ratio
   - Disc agent (yellow): Can jump
   - Diamonds (orange): The collectible items
   - Platforms (black): Define the level structure

2. **Game State Representation**:
   The game state is represented as a string with the following format:

   ```lisp
   ((:RECT x y width height rotation #diamonds_collected)
    (:DISC x y radius #diamonds_collected)
    (:DIAMOND x y)
    (:PLATFORM x1 y1 x2 y2)
    ...)
   ```

3. **Communication**:
   - The game uses WebSockets for communication
   - Agents can send messages to the server
   - The server sends the game state to the agents

## Agent Implementation Approaches

### 1. Agent Architecture

Consider implementing your agent using a hybrid architecture that combines:

1. **Reactive Layer**:
   - Basic movement and collision avoidance
   - Simple diamond collection when in proximity

2. **Deliberative Layer**:
   - Path planning to reach diamonds
   - Cooperation planning with the other agent

3. **Learning Layer**:
   - Adapt to the other agent's behavior
   - Learn effective strategies for different level configurations

### 2. Agent Perception

Implement perception modules to:

1. **Identify Agent Type**:
   - Determine if your agent is the disc or rectangle
   - Understand your agent's capabilities

2. **Environment Mapping**:
   - Create a representation of the level layout
   - Identify reachable and unreachable areas

3. **Diamond Detection**:
   - Locate all diamonds in the environment
   - Classify diamonds as individually collectible or requiring cooperation

4. **Other Agent Tracking**:
   - Monitor the other agent's position and movements
   - Infer the other agent's intentions

### 3. Decision Making

Implement decision-making using:

1. **ACT-R Framework**:
   - Use production rules for reactive behaviors
   - Implement declarative memory for storing level information
   - Use procedural memory for action sequences

2. **Planning Algorithms**:
   - A* or similar algorithms for path planning
   - Hierarchical Task Network (HTN) planning for complex tasks

3. **Coordination Mechanisms**:
   - Explicit communication protocols with the other agent
   - Implicit coordination through observation and prediction

### 4. Movement and Action Control

Implement specialized movement strategies for each agent type:

1. **Disc Agent**:
   - Jumping mechanics and timing
   - Landing prediction and control

2. **Rectangle Agent**:
   - Width-height ratio adjustment strategies
   - Rotation control for navigating tight spaces

### 5. Cooperation Strategies

Develop cooperation strategies such as:

1. **Role-based Cooperation**:
   - Assign roles (e.g., supporter, collector) based on the situation
   - Switch roles dynamically as needed

2. **Spatial Cooperation**:
   - Position-based strategies (e.g., one agent creates a bridge for the other)
   - Timing-based strategies (coordinated movements)

3. **Resource Allocation**:
   - Decide which agent should collect which diamonds
   - Optimize for maximum total score

### 6. Learning and Adaptation

Implement learning mechanisms:

1. **Reinforcement Learning**:
   - Learn effective movement patterns
   - Adapt to the other agent's behavior

2. **Case-based Reasoning**:
   - Store successful solutions to similar problems
   - Adapt past solutions to new situations

3. **Online Parameter Tuning**:
   - Adjust parameters based on performance feedback
   - Optimize for different level configurations

## Implementation Steps

1. **Basic Agent Implementation**:
   - Implement perception of the environment
   - Develop basic movement controls
   - Create simple diamond collection behavior

2. **Individual Problem Solving**:
   - Implement path planning to reach diamonds
   - Develop strategies for navigating complex environments

3. **Cooperative Behavior**:
   - Implement communication mechanisms
   - Develop cooperative strategies
   - Test with different partner agents

4. **Learning and Adaptation**:
   - Implement learning mechanisms
   - Test adaptation to different scenarios

5. **Evaluation and Refinement**:
   - Create test levels to evaluate performance
   - Refine strategies based on performance metrics

## Evaluation Metrics

Evaluate your agent using:

1. **Performance Metrics**:
   - Number of diamonds collected
   - Time to collect diamonds
   - Success rate in different levels

2. **Cooperation Metrics**:
   - Number of cooperatively collected diamonds
   - Efficiency of cooperation
   - Adaptability to different partner agents

3. **Robustness Metrics**:
   - Performance across different level configurations
   - Recovery from failures
   - Adaptation to unexpected situations

## Potential Challenges and Solutions

1. **Challenge**: Determining which agent you control
   **Solution**: Implement a detection mechanism based on movement responses

2. **Challenge**: Coordinating with the other agent
   **Solution**: Develop both explicit and implicit coordination mechanisms

3. **Challenge**: Planning in a dynamic environment
   **Solution**: Implement adaptive planning that can adjust to changes

4. **Challenge**: Balancing reactivity and deliberation
   **Solution**: Use a hybrid architecture with appropriate switching mechanisms

5. **Challenge**: Handling uncertainty in physics simulation
   **Solution**: Implement robust control strategies with error correction

## Advanced Implementation Ideas

1. **Predictive Modeling**:
   - Predict the physics of the environment
   - Anticipate the other agent's actions

2. **Meta-reasoning**:
   - Reason about the effectiveness of different strategies
   - Allocate computational resources efficiently

3. **Explainable AI Techniques**:
   - Make the agent's decisions interpretable
   - Facilitate debugging and improvement

4. **Multi-agent Coordination Patterns**:
   - Implement established coordination patterns from multi-agent systems research
   - Adapt patterns to the specific constraints of the GeoMates environment

Remember that a well-justified approach with thorough analysis is valued more than simply winning the challenge. Focus on developing a conceptually sound agent with clear reasoning behind your design choices.

## ACT-R Implementation Examples

Below are key ACT-R code snippets that you can add to the model-dummy.lisp file to enhance your agent's capabilities. These examples focus on essential functionality for the GeoMates project.

### 1. Chunk Types for Environment Representation

```lisp
;; Define chunk types for environment elements
(chunk-type diamond-object id x y collected)
(chunk-type platform-object id x1 y1 x2 y2)
(chunk-type agent-state type x y width height rotation diamonds-collected)
(chunk-type game-state my-agent other-agent diamonds platforms)
```

### 2. Agent Type Detection

```lisp
;; Production rule to detect if the agent is a rectangle
(p detect-rectangle-agent
   =visual>
     isa visual-object
     color red
     width =w
     height =h
   ?goal>
     state free
==>
   +goal>
     isa goal
     state identify-agent
     agent-type rectangle
   +imaginal>
     isa agent-state
     type rectangle
     width =w
     height =h
   !output! (I am the rectangle agent)
)

;; Production rule to detect if the agent is a disc
(p detect-disc-agent
   =visual>
     isa visual-object
     color yellow
     width =w
     height =h
   ?goal>
     state free
==>
   +goal>
     isa goal
     state identify-agent
     agent-type disc
   +imaginal>
     isa agent-state
     type disc
     width =w
     height =h
   !output! (I am the disc agent)
)
```

### 3. Environment Perception and Mapping

```lisp
;; Production rule to detect diamonds in the environment
(p detect-diamond
   =visual>
     isa visual-object
     color orange
     screen-x =x
     screen-y =y
     value =id
   =goal>
     isa goal
     state scan-environment
==>
   =goal>
     isa goal
     state process-diamond
   +imaginal>
     isa diamond-object
     id =id
     x =x
     y =y
     collected nil
   !output! (Found diamond at =x =y)
)

;; Production rule to detect platforms in the environment
(p detect-platform
   =visual>
     isa visual-object
     color black
     screen-x =x1
     screen-y =y1
     width =w
     height =h
   =goal>
     isa goal
     state scan-environment
==>
   =goal>
     isa goal
     state process-platform
   +imaginal>
     isa platform-object
     x1 =x1
     y1 =y1
     x2 (+ =x1 =w)
     y2 (+ =y1 =h)
   !output! (Found platform at =x1 =y1 with width =w and height =h)
)
```

### 4. Basic Movement Controls

```lisp
;; Production rule for the rectangle agent to move left
(p rectangle-move-left
   =goal>
     isa goal
     state move
     direction left
   =imaginal>
     isa agent-state
     type rectangle
   ?manual>
     state free
==>
   =goal>
     state execute-move
   +manual>
     cmd press-key
     key "a"
   !output! (Moving rectangle left)
)

;; Production rule for the disc agent to jump
(p disc-jump
   =goal>
     isa goal
     state move
     direction up
   =imaginal>
     isa agent-state
     type disc
   ?manual>
     state free
==>
   =goal>
     state execute-move
   +manual>
     cmd press-key
     key "w"
   !output! (Disc jumping)
)

;; Production rule for the rectangle agent to change shape (flatten)
(p rectangle-flatten
   =goal>
     isa goal
     state change-shape
     shape flat
   =imaginal>
     isa agent-state
     type rectangle
   ?manual>
     state free
==>
   =goal>
     state execute-shape-change
   +manual>
     cmd press-key
     key "s"
   !output! (Flattening rectangle)
)
```

### 5. Diamond Collection Strategy

```lisp
;; Production rule to identify the closest diamond
(p find-closest-diamond
   =goal>
     isa goal
     state find-target
   =imaginal>
     isa agent-state
     x =agent-x
     y =agent-y
   =retrieval>
     isa diamond-object
     x =diamond-x
     y =diamond-y
     collected nil
==>
   =goal>
     state calculate-path
     target-x =diamond-x
     target-y =diamond-y
   !output! (Targeting diamond at =diamond-x =diamond-y)
)

;; Production rule to move toward a diamond
(p move-toward-diamond
   =goal>
     isa goal
     state move-to-target
     target-x =tx
     target-y =ty
   =imaginal>
     isa agent-state
     x =x
     y =y
==>
   =goal>
     state determine-direction
   !eval! (calculate-direction =x =y =tx =ty)
   !output! (Moving toward diamond at =tx =ty)
)
```

### 6. Cooperation Mechanisms

```lisp
;; Production rule to detect when the other agent is nearby
(p detect-other-agent-nearby
   =visual>
     isa visual-object
     ; This will be either yellow or red, opposite of our agent
     color =other-color
     screen-x =ox
     screen-y =oy
   =imaginal>
     isa agent-state
     x =x
     y =y
   =goal>
     isa goal
     state scan-environment
   !eval! (nearby =x =y =ox =oy 5)  ; Check if within 5 units
==>
   =goal>
     state coordinate-with-other
   +imaginal>
     isa coordination
     other-x =ox
     other-y =oy
   !output! (Other agent is nearby at =ox =oy)
)

;; Production rule to form a platform for the other agent
(p form-platform-for-other
   =goal>
     isa goal
     state coordinate-with-other
   =imaginal>
     isa agent-state
     type rectangle
   =retrieval>
     isa coordination
     other-x =ox
     other-y =oy
     other-needs-platform yes
==>
   =goal>
     state move-to-position
     target-x =ox
     target-y (- =oy 2)  ; Position below the other agent
   !output! (Moving to form platform for other agent)
)
```

### 7. Learning Mechanisms

```lisp
;; Production rule to learn from successful diamond collection
(p learn-successful-collection
   =goal>
     isa goal
     state evaluate-action
     action collect-diamond
     success t
   =imaginal>
     isa strategy
     method =method
==>
   =goal>
     state update-strategy
   =imaginal>
     success-count (+ =success-count 1)
   !eval! (reinforce-production =method)
   !output! (Learning from successful diamond collection using =method)
)

;; Production rule to adapt to failed attempts
(p adapt-to-failure
   =goal>
     isa goal
     state evaluate-action
     action =action
     success nil
   =imaginal>
     isa strategy
     method =method
==>
   =goal>
     state try-alternative
   =imaginal>
     failure-count (+ =failure-count 1)
   !eval! (find-alternative-strategy =method)
   !output! (Adapting strategy after failure of =method)
)
```

### 8. Communication with Other Agent

```lisp
;; Production rule to send a message to the other agent
(p send-coordination-message
   =goal>
     isa goal
     state coordinate-with-other
     coordination-type =type
   =imaginal>
     isa coordination
     target-x =tx
     target-y =ty
==>
   =goal>
     state wait-for-response
   !eval! (send-message (create-coordination-message =type =tx =ty))
   !output! (Sending coordination message: =type at =tx =ty)
)

;; Production rule to process a message from the other agent
(p process-coordination-message
   =goal>
     isa goal
     state process-message
   =retrieval>
     isa message
     message-type coordination
     coordination-type =type
     target-x =tx
     target-y =ty
==>
   =goal>
     state respond-to-coordination
     coordination-type =type
     target-x =tx
     target-y =ty
   !output! (Received coordination request: =type at =tx =ty)
)
```

### 9. Utility Functions (to be defined in Lisp)

```lisp
;; Calculate direction to move based on current and target positions
(defun calculate-direction (current-x current-y target-x target-y)
  "Determine which direction to move based on current and target positions"
  (let ((dx (- target-x current-x))
        (dy (- target-y current-y)))
    (cond
      ((> (abs dx) (abs dy))
       (if (> dx 0) 'move-right 'move-left))
      (t
       (if (> dy 0) 'move-up 'move-down)))))

;; Check if two positions are nearby
(defun nearby (x1 y1 x2 y2 threshold)
  "Check if two positions are within threshold distance of each other"
  (<= (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)) (expt threshold 2)))

;; Create a coordination message
(defun create-coordination-message (type x y)
  "Create a message to coordinate with the other agent"
  (format nil "COORD:~A:~A:~A" type x y))

;; Parse a received message
(defun parse-message (message)
  "Parse a received message and create appropriate chunks"
  (let* ((parts (split-string message ":"))
         (msg-type (first parts)))
    (cond
      ((string= msg-type "COORD")
       (let ((coord-type (second parts))
             (x (parse-number (third parts)))
             (y (parse-number (fourth parts))))
         (create-message-chunk coord-type x y)))
      (t (create-unknown-message-chunk message)))))
```

### 10. Integration with Game State

```lisp
;; Production rule to process the game state
(p process-game-state
   =goal>
     isa goal
     state process-environment
   =visual>
     isa game-state
     rect-x =rx
     rect-y =ry
     rect-width =rw
     rect-height =rh
     disc-x =dx
     disc-y =dy
     disc-radius =dr
==>
   =goal>
     state update-knowledge
   +imaginal>
     isa game-state
     rect-agent-x =rx
     rect-agent-y =ry
     rect-agent-width =rw
     rect-agent-height =rh
     disc-agent-x =dx
     disc-agent-y =dy
     disc-agent-radius =dr
   !output! (Updated game state knowledge)
)
```

These code snippets provide a foundation for implementing the key aspects of your GeoMates agent. You can integrate them into the model-dummy.lisp file and adapt them to your specific implementation needs. Remember to also implement the supporting utility functions and ensure proper integration with the ACT-R framework.

## Recommended Implementation Workflow

1. Start by implementing the agent type detection and basic movement controls
2. Add environment perception and mapping capabilities
3. Implement diamond collection strategies
4. Add cooperation mechanisms
5. Integrate learning and adaptation features
6. Test and refine your agent in various scenarios

Remember that a well-justified approach with thorough analysis is valued more than simply winning the challenge. Focus on developing a conceptually sound agent with clear reasoning behind your design choices.
