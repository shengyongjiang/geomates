
# self location

## who am i

```plain
@model-dummy.lisp 
In model-dummy.lisp, I need to implement a self-identification mechanism since I don't know whether I'm controlling the yellow disc or red block. My approach is to:
1 Record the initial position of the yellow disc
2 Record the initial position of the red block
3 Issue a movement command (move left)
4 Check if the yellow disc position has changed
5 Check if the red block position has changed
Whichever character moved in response to my command is the one I'm controlling. This detection method allows me to adapt my strategy accordingly without relying on explicit identification messages from the server.
give me possbile production for step 1
```