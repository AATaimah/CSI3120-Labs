% Main rule for character description
character_description -->
    character_type(Type),
    character_subtype(Type),
    sequence,
    movement_direction(Direction),
    health_level,
    weapon(Type, Weapon),
    { valid_movement(Type, Weapon, Direction) },
    movement_style.

% Character types
character_type(enemy) --> [enemy].
character_type(hero)  --> [hero].

% Character subtypes
character_subtype(enemy) --> [darkwizard].
character_subtype(enemy) --> [demon].
character_subtype(enemy) --> [basilisk].
character_subtype(hero)  --> [wizard].
character_subtype(hero)  --> [mage].
character_subtype(hero)  --> [elf].

% Sequence number (positive integer)
sequence --> [Sequence], { integer(Sequence), Sequence > 0 }.

% Movement directions
movement_direction(towards) --> [towards].
movement_direction(away)    --> [away].

% Health levels
health_level --> [very_weak].
health_level --> [weak].
health_level --> [normal].
health_level --> [strong].
health_level --> [very_strong].

% Weapon possession rules
weapon(enemy, no_weapon) --> [no_weapon].
weapon(hero, Weapon) --> [Weapon], { member(Weapon, [has_weapon, no_weapon]) }.

% Valid movement based on character type and weapon possession
valid_movement(enemy, _, towards).
valid_movement(hero, has_weapon, towards).
valid_movement(hero, no_weapon, away).

% Movement styles
movement_style --> [jerky].
movement_style --> [stealthy].
movement_style --> [smoothly].
