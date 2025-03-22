### English Version

Yes, you can easily relaunch the GeoMates game within your Docker setup without rebuilding the entire container or manually restarting the process each time. Since you’re using Docker Compose and a Dockerfile to set up the environment, here are several approaches to achieve this, leveraging the existing configuration:

#### 1. Restart the Container with Docker Compose
The simplest way to relaunch the game is to restart the running container, which will re-execute the `sbcl --script geomates.lisp` command specified in your `docker-compose.yml`. This resets the game to its initial state (starting from the first level in `levels.lisp`).

- **Steps**:
  - Ensure the container is running:
    ```bash
    docker-compose up -d
    ```
  - Restart the container:
    ```bash
    docker-compose restart geomates
    ```
  - This stops and restarts the `geomates` service, effectively relaunching the game server. The `-d` flag keeps it running in detached mode.

- **Why It Works**:
  - The `command: sbcl --script geomates.lisp` in `docker-compose.yml` runs the `main` function in `geomates.lisp`, which initializes the Box2D world and starts the game loop. Restarting the container re-runs this command, resetting all game state (e.g., levels, agent connections, and GUI).

- **Limitations**:
  - If the SBCL process hangs or doesn’t exit cleanly (e.g., due to unhandled errors), you might need to force a stop and start instead:
    ```bash
    docker-compose stop geomates
    docker-compose start geomates
    ```

#### 2. Modify the Game to Support Relaunch Without Restart
You can modify `geomates.lisp` to add a relaunch mechanism within the running process, avoiding the need to restart the container. This involves adding a command that agents or the GUI can send to reset the game state.

- **Implementation**:
  - Add a reset command (e.g., `r`) to the game loop in `main`. Here’s a minimal modification:
    ```lisp
    (defun reset-game (level-list)
      (destroyworld) ; Clean up the current Box2D world
      (initialize-box2d) ; Reinitialize Box2D
      (setf diamonds-disc 0 diamonds-rect 0) ; Reset scores
      (setup-level (first level-list))) ; Reset to first level

    ;; Inside the main function’s level loop, add this condition:
    (when (and (listen disc-agent-stream) (eql (read-byte disc-agent-stream nil nil) 114)) ; 'r' = 114 in ASCII
      (reset-game *levels*)
      (format info-stream "game reset~%")
      (finish-output info-stream))
    ```
  - Update the Docker volume to reflect the modified file:
    - Since `./geomates/geomates.lisp` isn’t mounted in your `docker-compose.yml`, you’d need to rebuild the image after modifying the file locally:
      ```bash
      docker-compose up --build -d
      ```
    - Alternatively, add it to the `volumes` section:
      ```yaml
      - ./geomates/geomates.lisp:/usr/src/app/geomates/geomates.lisp
      ```
      Then restart the container to load the updated file:
      ```bash
      docker-compose restart geomates
      ```

- **Usage**:
  - Connect to the game server (e.g., via telnet or an ACT-R agent) and send the `r` character to reset the game. This keeps the container running while resetting the game state.

- **Advantages**:
  - No need to stop/restart the container, preserving active connections until the reset is triggered.

#### 3. Use Docker Exec to Restart SBCL Inside the Container
If you want to relaunch the game without restarting the container, you can manually restart the SBCL process inside the running container.

- **Steps**:
  - Get the container ID:
    ```bash
    docker ps
    ```
  - Execute a new SBCL instance inside the container:
    ```bash
    docker exec -it <container_id> sbcl --script /usr/src/app/geomates/geomates.lisp
    ```
  - Note: This won’t kill the existing SBCL process unless you stop it first. To stop the running process:
    ```bash
    docker exec <container_id> pkill sbcl
    ```
    Then run the command above to relaunch.

- **Limitations**:
  - This approach is manual and requires you to manage the process lifecycle. It’s less seamless than restarting the container.

#### Recommended Approach
For ease of use with your current setup, **Option 1 (Restart with Docker Compose)** is the most straightforward and aligns with Docker’s design. If you need more control or frequent resets during development, consider **Option 2 (Modify the Game)** to integrate a reset command, which can be triggered programmatically by agents or the GUI.

---

### Chinese Version

是的，您可以在Docker设置中轻松重新启动GeoMates游戏，而无需重建整个容器或每次手动重启进程。由于您使用Docker Compose和Dockerfile来设置环境，以下是几种利用现有配置实现这一目标的方法：

#### 1. 使用Docker Compose重启容器
重新启动游戏的最简单方法是重启运行中的容器，这将重新执行`docker-compose.yml`中指定的`sbcl --script geomates.lisp`命令。这会将游戏重置到初始状态（从`levels.lisp`中的第一个关卡开始）。

- **步骤**：
  - 确保容器正在运行：
    ```bash
    docker-compose up -d
    ```
  - 重启容器：
    ```bash
    docker-compose restart geomates
    ```
  - 这会停止并重启`geomates`服务，有效地重新启动游戏服务器。`-d`标志使其在分离模式下运行。

- **为什么有效**：
  - `docker-compose.yml`中的`command: sbcl --script geomates.lisp`运行`geomates.lisp`中的`main`函数，该函数初始化Box2D世界并启动游戏循环。重启容器会重新运行此命令，重置所有游戏状态（例如关卡、代理连接和GUI）。

- **限制**：
  - 如果SBCL进程挂起或未正常退出（例如由于未处理的错误），您可能需要强制停止并启动：
    ```bash
    docker-compose stop geomates
    docker-compose start geomates
    ```

#### 2. 修改游戏以支持无需重启的重新启动
您可以修改`geomates.lisp`，在运行进程中添加重新启动机制，避免重启容器。这涉及添加一个代理或GUI可以发送的命令来重置游戏状态。

- **实现**：
  - 在`main`的游戏循环中添加重置命令（例如`r`）。以下是最小修改：
    ```lisp
    (defun reset-game (level-list)
      (destroyworld) ; 清理当前Box2D世界
      (initialize-box2d) ; 重新初始化Box2D
      (setf diamonds-disc 0 diamonds-rect 0) ; 重置分数
      (setup-level (first level-list))) ; 重置到第一个关卡

    ;; 在main函数的关卡循环中，添加此条件：
    (when (and (listen disc-agent-stream) (eql (read-byte disc-agent-stream nil nil) 114)) ; 'r' = 114 in ASCII
      (reset-game *levels*)
      (format info-stream "game reset~%")
      (finish-output info-stream))
    ```
  - 更新Docker卷以反映修改后的文件：
    - 由于`./geomates/geomates.lisp`未在您的`docker-compose.yml`中挂载，您需要在本地修改文件后重建镜像：
      ```bash
      docker-compose up --build -d
      ```
    - 或者，将其添加到`volumes`部分：
      ```yaml
      - ./geomates/geomates.lisp:/usr/src/app/geomates/geomates.lisp
      ```
      然后重启容器以加载更新文件：
      ```bash
      docker-compose restart geomates
      ```

- **使用**：
  - 连接到游戏服务器（例如通过telnet或ACT-R代理）并发送`r`字符以重置游戏。这保持容器运行，同时重置游戏状态。

- **优势**：
  - 无需停止/重启容器，在触发重置前保持活动连接。

#### 3. 使用Docker Exec在容器内重启SBCL
如果您想在不重启容器的情况下重新启动游戏，可以在运行的容器内手动重启SBCL进程。

- **步骤**：
  - 获取容器ID：
    ```bash
    docker ps
    ```
  - 在容器内执行新的SBCL实例：
    ```bash
    docker exec -it <container_id> sbcl --script /usr/src/app/geomates/geomates.lisp
    ```
  - 注意：这不会终止现有的SBCL进程，除非您先停止它。要停止运行的进程：
    ```bash
    docker exec <container_id> pkill sbcl
    ```
    然后运行上述命令重新启动。

- **限制**：
  - 这种方法是手动的，需要您管理进程生命周期。不如重启容器无缝。

#### 推荐方法
对于当前设置的易用性，**选项1（使用Docker Compose重启）**是最直接的，符合Docker的设计。如果您在开发过程中需要更多控制或频繁重置，请考虑**选项2（修改游戏）**以集成重置命令，可通过代理或GUI程序化触发。
