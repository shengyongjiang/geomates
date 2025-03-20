# Practical for Intelligent cooperative Agents WS24/

## Description of practical

Your task is to build an agent for cooperative physical simulation game GeoMates, a simplified
clone of “geometry friends” that ran as competition at the IJCAI conference. Your agent needs to
be able to interact with the environment in order to collect diamonds and to interact with the
other agent – we will set up an ACT-R environment so you can handle the agent similar to the
tutorial example of the agent.
There will be different trials that will need different solutions. In the end we will have a challenge
with new problems to be solved – there will not be completely new components but configured
differently, i.e., your agent should not depend on fixed physical parameters.

## The form of the practical

### The task

Your agent can be either the disc (yellow) or the block (red) – each has different motion modes
(disc jumps, block changes its width-to-height ratio). Your goal is to catch the diamonds (orange) in
order to maximize the number of points you earn. While some of the diamonds can be reached by
an agent on its own, others may require collaboration among the agents to reach them. Your agent
receives 2 points for each diamond collected, plus one point for any diamond collected by the
other agent. The task comprises the following challenges:

1) finding out which character your agent controls and what motion capabilities it has
2) understanding the level: How can it be solved? Are there different options?
3) interaction with the other agent
4) design of an agent: What techniques discussed in the course could be helpful?


have regular checkpoints where you can present your concepts and problems and solutions will be
discussed.

### Technical details

You can obtain the Geomates environment by cloning our repository <https://gitlab.isp.uniluebeck.de/hai/geomates->
 (please use git to ease receiving updates as we may need to fix bugs).
The Readme document explains how to install the software, please read it carefully before posting questions. We provide source code and a docker file for virtualization.

There will be a dummy Act-R agent as starting point. You are allowed to include external
components as well (e.g., a PDDL planning system, theorem prover, etc.). In the agent code, there is
an example of how regular code or external software can be connected. In order to allow us
running your agent, the following requirements must be met for any agent that goes beyond pure
Act-R:

- software must be able to run in standard *nix environments
- only free-to-use external software also available as source code allowed (e.g., no Matlab, no
    proprietary software)
- being able to provide a docker container for your agent

### Timetable

1. Find a group of up to three. In case you want to work by your own – that is ok but will not
    be credited  advice is to work with others (find people that have similar available time
    slots for getting into exchange frequently to organize your work)
    Due to limited resources for providing guidance, we will face difficulties to assist students that want to work individually on the project; be advised to work in groups.
2. First, try the task by yourself and report to each other how you do the task and then discuss
    how you want your agents to work on the task. Review the topics discussed in class. There
    are a lot of different possibilities, but likely no approach that can be realized in the given
    time will master all situations. Discuss which will offer the best compromise for the project,
    in your groups’ opinion.
3. Prepare a written exposé/concept of your approach (including the team members) of
    about one to two pages by **2025 -02-12** (it can be reused for your project report). Upload
    the exposé to Moodle. The exposé should address the following questions:

    **a** What is the main idea of your agent, i.e., what are the main principles according
        which the agent will work on the task and how do they relate to course topics?
        (e.g., which Act-R mechanisms or AI techniques will it include)

    **b** How will the agent decide to approach the diamonds, how will it plan actions?

    **c** How will the agent try to collaborate?

    **d** What will the agent learn? (e.g., who am I, was my action successful, how the other
        agent responds)

    **e** How do you plan to evaluate your agent? What are levels in which the agent is
        expected to perform particularly well?

    **f** working in the group: How will you work together in the group and implement the
        agent? How will you organize the exchange so you can organize and coordinate
        your work as to make sure everybody knows how the agent works and how it can
        be improved? What might be challenges for the group work?

    **g** Give a rough breakdown of your project in terms of work packages you will tackle.

4. We will provide feedback to your concept in written form or in a video call.
5. Submit your final agent by **11th of March** (preferably a draft version earlier to validate the
software is running on our server). We will run a competition on 20th of February by
pairing your agents randomly and challenging them with unseen levels (a live stream is
planned).
6. Submit your final project report **by 23rd of March.** Your project report should not exceed
10 pages, it may be augmented by a video presentation. The report should include:

    **a** description from how your agent works (e.g., taken from the exposé);

    **b** reflections on how the project evolved (e.g., unanticipated complications);

    **c** experimental results achieved with your agent (e.g., performance in levels)

### The challenge will be on 3 rd of March

Grading of the final agent and report handed in at the end will be based on the following criteria:

1. appropriateness and difficulty of the concepts selected for your agent: the approach you
    develop should be well thought through, consider relevant aspects, and be realistic to achieve;
2. clarity of technical description with exactly all details that are relevant;
3. the agent is running and is able to solve some of the problems (either in isolation, better in
    cooperation);
4. development of own levels for evaluating the strengths and weaknesses of your agent;
5. the evaluation and analyses of the agent behavior, also reflecting your initial goals.

Agent performance in the challenge is not part of the grading. We acknowledge that some ideas
will be more difficult to implement than others. Therefore, we will balance the expectations on
discussion, experimentation, and analysis with the effort required to implement a specific
approach. A winning agent without a good description and analysis will not give you a better
grade than a mediocre agent based on a well-justified concept and in-depth analyses.

### The challenge

In addition to some problems provided, we will give you during the practical, we will add new
problems (not too different) that the agents need to solve.

   1) The agent by itself
   2) The two agents together
   3) A combination of agents from different groups

