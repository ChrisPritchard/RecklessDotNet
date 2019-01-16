# Design of Reckless.Net

## Interaction

Map is presented.

- Player can see and inspect offices and market tiles
- Player can issue an order, optionally requiring the selection of a building and a target (tile or another building)
- Player can end the turn, and is presented a summary of their action/results on the next turn start

## Display

Simple isometric view with tiles presented. Tiles are abstract images, coloured by owner. Offices are vertical buildings, likewise coloured by owner.
Parent ownership is another tile, and head office is an icon on each office.

Needed assets:

- tile
- office
- link (vertical)
- link (horizontal)
- hq icon

Needed interface:

- inspection interface
- end turn button: idle, hover, pressed
- order interface

### Camera object?

- updated in advance model, picking up left/right/up/down
- results in an offset set of creds
- main view (tiles, offices etc) draw relative to this set of coords

A camera might not be necessary: original RC had a fixed view

## Mouse to tile

## Tile popups

1. If the mouse is pressed then:
    - eliminate the previous popup, if open
    - work out if a mouse tile is pressed
    - if so, generate ui and set
    - ui is a list, doesnt need to be a specific record