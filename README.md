# UI

## Outline

## Team Members and their Roles

| Team Member           |     Role     |
| --------------------- | :----------: |
| Florian Koudjonou     | Scrum Master |
| Anne-Sophie Cusson    |  Developper  |
| Jean-Sébastien Demers |  Developper  |
| Basma Kaanane         |  Developper  |

These roles may evolve or change throughout the project, but we have decided not to start the project with very strict roles.

## Objectives

#### Benefit to Customer

#### Key Things to Accomplish

#### Criterai for success

## Expected/Anticipated Architecture

## Anticipated Risks

## Legal and social issues

## Initial Plans for Release

We currently have two github repositories for the project. We have the ui repo which holds the software that will run the user interface on the host pc, and the firmware repo that will do data collection on the stm32mp157f-dk2 development board.

As we are all new to ADA and the development board, we will start by writing small test programs to get comfortable with the technologies. Afterwards, we will start working on the ui and firmware modules separately. For the first release of the ui module, we will likely feed the graphical interface dummy data. Same idea for the firmware module, we will make the board collect data without sending it anywhere. Once we have made good progress on both modules, we will work on making them communicate with each other.

Our goal is to make a basic oscilloscope. Once that goal is achieved, we will work on additional features.

In terms of tool setup, the host pc will be running linux (likely ubuntu) and the development board will be running linux as well (version tbd). We also need to install an ada compiler on both devices (gnat).
