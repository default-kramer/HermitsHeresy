#lang racket

{module+ test
  (require hermits-heresy rackunit
           (only-in "../hermits-heresy/hermits-heresy/simple.rkt" simple?)
           (rename-in (submod "../hermits-heresy/hermits-heresy/blockdata/blockdef.rkt" for-tests)
                      [all-symbols currently-defined-symbols]))

  ; I want to make sure I don't accidentally remove a block symbol that was working at some point
  (define historically-defined-symbols '(Bedrock
                                         Earth
                                         Grassy-Earth
                                         Limegrassy-Earth
                                         Tilled-Soil
                                         Clay
                                         Mossy-Earth
                                         Chalk
                                         Chunky-Chalk
                                         Obsidian
                                         Sand
                                         Sandstone
                                         Sandy-Sandstone
                                         Bloodstone
                                         Ash
                                         Witchgrassy-Earth
                                         Poisonous-Peat
                                         Snow-Cover
                                         Snow
                                         Ice
                                         Clodstone
                                         Crumbly-Clodstone
                                         Basalt
                                         Translucent-Block
                                         Lava
                                         Vault-Wall
                                         Viny-Vault-Wall
                                         Cracked-Vault-Wall
                                         Mud-Brick-Wall
                                         Timbered-Foundation
                                         Timbered-Wall-Midsection
                                         Timbered-Capital
                                         Hardwood-Pedestal
                                         Hardwood-Column-Midsection
                                         Hardwood-Capital
                                         Stone-Foundation
                                         Stone-Wall-Midsection
                                         Castle-Foundation
                                         Castle-Wall-Midsection
                                         Castle-Crenellation
                                         Castle-Battlement
                                         Wooden-Wall
                                         Plumberry-Lumber
                                         Flagstone
                                         Straw-Floor
                                         Wooden-Floor
                                         Hardwood-Tile
                                         Purple-Flagstone
                                         Blue-Flagstone
                                         Carpet-Midsection
                                         Fancy-Carpet
                                         Castle-Tile
                                         Carved-Castle-Tile
                                         Bronze-Temple-Tile
                                         Blue-Temple-Tile
                                         Golemite
                                         Coal-Vein
                                         Copper-Vein
                                         Iron-Vein
                                         Silver-Vein
                                         Gold-Vein
                                         Zenithium-Vein
                                         Ruby-Vein
                                         Mythril-Vein
                                         Orichalcum-Vein
                                         Palm-Lumber
                                         Cedar-Lumber
                                         Time-Honoured-Turf
                                         8-Bit-Brick
                                         Olde-Worlde-Wall
                                         Eyewall
                                         Dummy-Block
                                         Lemongrassy-Earth
                                         Unused-Ore
                                         Strange-Sand
                                         Unused-Lava-Block
                                         Unused-Ice-Block
                                         Bonestone
                                         Hargon-Block
                                         Slimy-Block
                                         Retro-Roof
                                         Black-Block
                                         Grey-Block
                                         White-Block
                                         Burgundy-Block
                                         Red-Block
                                         Purple-Block
                                         Pink-Block
                                         Green-Block
                                         Lime-Green-Block
                                         Orange-Block
                                         Yellow-Block
                                         Navy-Block
                                         Blue-Block
                                         Beige-Block
                                         Cyan-Block
                                         Silver-Block
                                         Gold-Block
                                         Fortress-Foundation
                                         Fortress-Wall-Midsection
                                         Fortress-Battlement
                                         Clear-water-full-block
                                         Clear-water-surface-block
                                         Witchgrassy-Block
                                         Lemongrassy-Block
                                         Grassy-Block
                                         Limegrassy-Block
                                         Light-Dolomite
                                         Dark-Dolomite
                                         Stony-Sand
                                         Masonry-Wall
                                         Mossy-Bark
                                         Bark
                                         Stony-Soil
                                         Clear-water-shallow-block
                                         Seaside-Sand
                                         Arid-Earth
                                         Chert
                                         Chunky-Chert
                                         Leaves
                                         Spoiled-Soil
                                         Moss
                                         Humus
                                         Tilled-Humus
                                         Citadel-Battlement
                                         Golden-Citadel-Capital
                                         Alcoved-Citadel-Wall
                                         Golden-Citadel-Foundation
                                         Sanguine-Cinders
                                         Scales
                                         Bubbling-Seaside-Sand
                                         Patterned-Citadel-Wall
                                         Citadel-Capital
                                         Citadel-Foundation
                                         Citadel-Floor
                                         Poison-shallow-block
                                         Poison-surface-block
                                         Poison-full-block
                                         Bottomless-Swamp-shallow-block
                                         Bottomless-Swamp-surface-block
                                         Bottomless-Swamp-full-block
                                         Muddy-Water-shallow-block
                                         Muddy-Water-surface-block
                                         Muddy-Water-full-block
                                         Umber
                                         Umber-Sandstone-Block
                                         Hot-Water-shallow-block
                                         Hot-Water-surface-block
                                         Hot-Water-full-block
                                         Mossy-Spoiled-Soil
                                         Swampy-Soil
                                         Seeded-Spoiled-Soil
                                         Lumpy-Umber
                                         Softwood
                                         Floorboard
                                         Yellow-frame-block
                                         Lava-shallow-block
                                         Lava-surface-block
                                         Patterned-Vault-Wall
                                         Shifting-Sand
                                         Sea-water-shallow-block
                                         Sea-water-surface-block
                                         Sea-water-full-block
                                         Hot-water-shallow-block
                                         Hot-water-surface-block
                                         Plasma-shallow-block
                                         Plasma-surface-block
                                         Plasma-full-block
                                         Vault-Tile
                                         Gold-Brick
                                         Spaceship-Wall
                                         Shiny-Spaceship-Wall
                                         Golden-Tile
                                         Black-Void
                                         Battlefield-Earth
                                         Woodless-Timbered-Wall
                                         Banded-Timbered-Wall
                                         Red-Brick-Wall
                                         Diamond-Vein
                                         Chert-Red
                                         Citadel-Floor-Red
                                         Magnetite-Vein
                                         Siltstone
                                         Red-Brick-Floor
                                         Crazing-Paving
                                         Copper-Wall
                                         Marble
                                         Silver-Birch-Lumber
                                         Dark-Bark
                                         Teal-Leaves
                                         Bloodstained-Soil
                                         Bloodstained-Soil-full
                                         Padded-Floor
                                         Leafy-Spoiled-Soil
                                         Leafy-Spoiled-Soil-full
                                         Cracked-Floorboard
                                         Blackened-Bark
                                         Malachite
                                         Adobe-Capital
                                         Adobe-Wall-Midsection
                                         Adobe-Pedestal
                                         Alcoved-Adobe-Wall
                                         Copper-Floor
                                         Sooty-Softwood
                                         Heartwood
                                         White-Dye-Vein
                                         Black-Dye-Vein
                                         Purple-Dye-Vein
                                         Pink-Dye-Vein
                                         Red-Dye-Vein
                                         Green-Dye-Vein
                                         Yellow-Dye-Vein
                                         Blue-Dye-Vein
                                         Blue-Temple-Tile-Half-Dummy
                                         Carpet
                                         Golden-Castle-Capital
                                         Alcoved-Castle-Wall
                                         Golden-Castle-Foundation
                                         Cracked-Castle-Tile
                                         Citadel-Wall-Midsection
                                         Ghost-Block
                                         Plaster
                                         Leafy-Stony-Soil
                                         Leafy-Stony-Soil-Full
                                         Iron-Block
                                         Concrete-Block
                                         Sun-Sigil-Block
                                         Star-Sigil-Block
                                         Moon-Sigil-Block
                                         Water-Sigil-Block
                                         Soul-Sigil-Block
                                         Sandstone-Brick
                                         Withered-Leaves
                                         Sanguine-Sandstone
                                         Sanguine-Sand
                                         Sanguine-Sandy-Sandstone
                                         Silver-Brick
                                         Sooty-Silver-Brick
                                         Silver-Tile
                                         Seeded-Limegrassy-Earth
                                         Seeded-Mossy-Spoiled-Soil
                                         Seeded-Sand
                                         Seeded-Earth
                                         Seeded-Snow
                                         Seeded-Grassy-Earth
                                         Gravel-Block
                                         Scaled-Wall-Block
                                         Slatted-Wall-Block
                                         Woven-Straw-Block
                                         Polka-Dot-Block
                                         Faux-Floral-Floor-Block
                                         Palm-Patterned-Block
                                         Fruity-Fiend-Block
                                         Chequered-Block
                                         Sunset-Styled-Block
                                         Seaweed-Styled-Block
                                         Stylish-Stripy-Block
                                         Seaside-Scene-Block
                                         Abstract-Floral-Floor-Block
                                         Herringbone-Floorboard-Block
                                         Night-Sky-Wall-Block
                                         Granite-Floor-Block
                                         Stucco-Wall-Block
                                         Modern-Masonry-Block
                                         Chintzy-Block
                                         Gingham-Carpet
                                         Stripy-Wall
                                         Mottled-Wall
                                         Slimy-Wall
                                         Diamond-Design-Block
                                         Old-Skool-Wall-Block
                                         Art-Deco-Block
                                         Jungle-Wall
                                         Vintage-Wall
                                         Damask-Design-Block
                                         Refined-Design-Block
                                         Classy-Cross-Block
                                         ; OOPS - gonna go ahead and make a breaking change.
                                         ; Probably none of these should have been included.
                                         ; https://github.com/default-kramer/HermitsHeresy/issues/11
                                         #;(oops lets pretend these were never included:
                                                 Clear-Water-Full-Block
                                                 Clear-Water-Shallow-Block
                                                 Clear-Water-Surface-Block
                                                 Clear-Water-Small-Block
                                                 Hot-Water-Full-Block
                                                 Hot-Water-Shallow-Block
                                                 Hot-Water-Surface-Block
                                                 Hot-Water-Small-Block
                                                 Poison-Full-Block
                                                 Poison-Shallow-Block
                                                 Poison-Surface-Block
                                                 Poison-Small-Block
                                                 Liquid-Lava-Full-Block
                                                 Liquid-Lava-Shallow-Block
                                                 Liquid-Lava-Surface-Block
                                                 Liquid-Lava-Small-Block
                                                 Bottomless-Swamp-Full-Block
                                                 Bottomless-Swamp-Shallow-Block
                                                 Bottomless-Swamp-Surface-Block
                                                 Bottomless-Swamp-Small-Block
                                                 Filthy-Water-Full-Block
                                                 Filthy-Water-Shallow-Block
                                                 Filthy-Water-Surface-Block
                                                 Filthy-Water-Small-Block
                                                 Sea-Water-Full-Block
                                                 Sea-Water-Shallow-Block
                                                 Sea-Water-Surface-Block
                                                 Sea-Water-Small-Block
                                                 Plasma-Full-Block
                                                 Plasma-Shallow-Block
                                                 Plasma-Surface-Block
                                                 Plasma-Small-Block)))
  (for ([sym historically-defined-symbols])
    (check-true (integer? (block sym))))

  ; Make sure all current symbols are in the list
  (for ([sym currently-defined-symbols])
    (check-true (and (member sym historically-defined-symbols) #t)))


  ; Make sure that all valid symbols resolve to a simple? block ID.
  ; More specifically, make sure that (block 'some-liquid) returns the simple ID
  ; and not one of the item ID variants of that liquid.
  (for ([sym historically-defined-symbols])
    (check-true (simple? (block sym))))


  ; I've found some strange stuff about the liquids I don't understand yet.
  ; For example, 'Clear-water-full-block was originally defined as 120 or 128,
  ; and 120 was chosen arbitrarily. But when I filled my aquarium with 120
  ; and placed an item in it, the aquarium emptied itself like when a flowing
  ; water source is cut off. Using block 128 behaved more like I expected.
  (check-equal? (block 'Clear-water-full-block) 128)
  (check-equal? (block 'Sea-water-full-block) 341)
  (check-equal? (block 'Sea-water-shallow-block) 349)
  }
