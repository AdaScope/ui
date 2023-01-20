# UI

## Outline

### [Objectives](#objectives)

## Team Members and their Roles

| Team Member           |            Role            |
| --------------------- | :------------------------: |
| Florian Koudjonou     | Scrum Master and developer |
| Anne-Sophie Cusson    |         Developer          |
| Jean-Sébastien Demers |         Developer          |
| Basma Kaanane         |         Developer          |

These roles may evolve or change throughout the project, but we have decided not to start the project with very strict roles.

## Objectives

#### Benefit to Customer

#### Key Things to Accomplish

#### Criteria for success

## Expected/Anticipated Architecture

We will build the entire firmware of the oscilloscope using Ada and SPARK. Our client wants us to use those two languages for their reliability.
That firmware will then be executed on an STM32MP157 board running OpenSTLinux.
The signals measured by the board will then be sent to another computer via UART.
That second computer will run another program for the UI. That program will also be written in Ada and/or SPARK and we will use a library called GtkAda for the UI.

## Anticipated Risks

## Legal and social issues

The main legal or social issues associated with our project would be reliability.

Oscilloscopes are used to perform tests and maintenance on devices in many fields where the reliability of the equipment is top priority. These industries include aerospace & defense, civil aviation, rail, and medical industries. An unreliable oscilloscope used in those industries could be responsible for life threatening incidents. Therefore, the highest standards of reliability, accuracy and quality must be met by the AdaScope Oscilloscope.

## Initial Plans for Release

We currently have two github repositories for the project. We have the ui repo which holds the software that will run the user interface on the host pc, and the firmware repo that will do data collection on the stm32mp157f-dk2 development board.

As we are all new to ADA and the development board, we will start by writing small test programs to get comfortable with the technologies. Afterwards, we will start working on the ui and firmware modules separately. For the first release of the ui module, we will likely feed the graphical interface dummy data. Same idea for the firmware module, we will make the board collect data without sending it anywhere. Once we have made good progress on both modules, we will work on making them communicate with each other.

Our goal is to make a basic oscilloscope. Once that goal is achieved, we will work on additional features.

In terms of tool setup, the host pc will be running linux (likely ubuntu) and the development board will be running linux as well (version tbd). We also need to install an ada compiler on both devices (gnat).
