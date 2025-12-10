# Copyright (c) 2023 Boston Dynamics, Inc.

import argparse
import math
import sys
import time
import random
import bosdyn.client
import bosdyn.client.estop
import bosdyn.client.lease
import bosdyn.client.util
import msvcrt
from bosdyn.api import estop_pb2
from bosdyn.client.estop import EstopClient
from bosdyn.client.robot_command import (
    RobotCommandBuilder, RobotCommandClient,
    block_until_arm_arrives, blocking_stand
)

ROBOT_IP = "192.168.80.3"
USERNAME = "admin"
PASSWORD = "c9gg3fpta69s"


def verify_estop(robot):
    client = robot.ensure_client(EstopClient.default_service_name)
    if client.get_status().stop_level != estop_pb2.ESTOP_LEVEL_NONE:
        raise Exception("Robot is estopped. Use estop example to release estop.")

def arm_joint_move_long_trajectory_example(config):

    bosdyn.client.util.setup_logging(config.verbose)

    sdk = bosdyn.client.create_standard_sdk("ArmJointDemoClient")
    robot = sdk.create_robot(ROBOT_IP)
    robot.authenticate(USERNAME, PASSWORD)
    #bosdyn.client.util.authenticate(robot)
    robot.time_sync.wait_for_sync()

    verify_estop(robot)

    lease_client = robot.ensure_client(bosdyn.client.lease.LeaseClient.default_service_name)

    with bosdyn.client.lease.LeaseKeepAlive(lease_client, must_acquire=True, return_at_exit=True):

        robot.logger.info("Powering on robot...")
        robot.power_on(timeout_sec=20)
        assert robot.is_powered_on()
        robot.logger.info("Robot powered on.")

        command_client = robot.ensure_client(RobotCommandClient.default_service_name)

        blocking_stand(command_client, timeout_sec=10)
        robot.logger.info("Robot standing.")

        # Deploy the arm
        arm_ready = RobotCommandBuilder.arm_ready_command()
        cmd_id = command_client.robot_command(arm_ready)
        block_until_arm_arrives(command_client, cmd_id)

        # Initial gripper setup
        robot.logger.info("Opening gripper fully...")
        command_client.robot_command(
            RobotCommandBuilder.claw_gripper_open_fraction_command(1.0)
        )
        time.sleep(2)

        robot.logger.info("Setting gripper to 0.4...")
        command_client.robot_command(
            RobotCommandBuilder.claw_gripper_open_fraction_command(0.4)
        )
        time.sleep(2)

        print("Press ENTER to toggle gripper cycling ON/OFF. Press Ctrl+C to quit.\n")

        update_rate = 50  # Hz
        dt = 1.0 / update_rate
        t0 = time.time()

        # Trajectory amplitudes
        gripper_base = 0.4
        gripper_amp = 0.25
        sh0_base = 0.0
        sh0_amp = 0.3
        el0_base = 1.8
        el0_amp = 0.15
        wr0_amp = 0.15
        wr1_amp = 0.1

        # Frequencies
        gripper_freq = 0.5
        sh0_freq = 0.1
        el0_freq = 0.08
        wr_freq = 0.12

        # Control variables
        running = False
        open_fraction = 0.4
        #increment = 0.1
        #last_update = time.time()

        while True:

            # Windows-safe keypress detection
            if msvcrt.kbhit():
                key = msvcrt.getch()
                if key == b'\r':  # Enter key
                    running = not running
                    if running:
                        print("Spot conversation started.")
                    else:
                        print("Spot conversation stopped. Closing gripper.")
                        arm_ready_cmd = RobotCommandBuilder.arm_ready_command()
                        command_client.robot_command(arm_ready_cmd)
                        gripper_close_cmd = RobotCommandBuilder.claw_gripper_open_fraction_command(open_fraction=0.0)
                        command_client.robot_command(gripper_close_cmd)

            if running:
                t = time.time() - t0

                # Smooth sine/cosine trajectories
                sh0 = sh0_base + sh0_amp * math.sin(sh0_freq * t * 2 * math.pi)
                el0 = el0_base + el0_amp * math.sin(el0_freq * t * 2 * math.pi)
                wr0 = wr0_amp * math.sin(wr_freq * t * 2 * math.pi)
                wr1 = wr1_amp * math.sin(wr_freq * t * 2 * math.pi + math.pi/2)
                gripper = gripper_base + gripper_amp * math.sin(gripper_freq * t * 2 * math.pi)

                # Send smooth joint command
                arm_cmd = RobotCommandBuilder.arm_joint_command(
                    sh0, -1.0, el0, 0.0, -0.5, wr0
                )
                command_client.robot_command(arm_cmd)

                # Gripper command
                gripper_cmd = RobotCommandBuilder.claw_gripper_open_fraction_command(open_fraction=gripper)
                command_client.robot_command(gripper_cmd)

            time.sleep(dt)


def main():
    parser = argparse.ArgumentParser()
    bosdyn.client.util.add_base_arguments(parser)
    options = parser.parse_args()
    arm_joint_move_long_trajectory_example(options)

if __name__ == "__main__":
    if not main():
        sys.exit(1)
