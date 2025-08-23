# Huge string input (example)
huge_string = """
? 1boy 2247685
? 1girls 4007005
? armor 90429
? balls 2020370
? belly button 105170
? below view 4670
? blonde eyebrows 4665
? blonde female 100520
? blonde hair 1255384
? blue lipstick 17980
? bob cut 52873
? breasts 6664167
? cherry blossoms 5063
? choker 254808
? clitoral hood 39254
? clitoris 220080
? closed eyes 471044
? cowgirl position 293564
? creampie 78031
? cum 2241721
? cum in pussy 683475
? earrings 330988
? eyebrows 191171
? eyelashes 342629
? eyeshadow 152328
? female 8792035
? from below 54098
? hair behind ear 2111
? jewelry 319558
? light skin 1161244
? light-skinned female 942408
? makeup 235394
? male 5521375
? medium breasts 450525
? medium hair 131184
? mole 100067
? navel 950108
? outdoors 275307
? parted lips 84679
? penis 4478176
? petals 9340
? pink pussy 20084
? pleasure face 126025
? purple eyeshadow 11464
? pussy 3290098
? reverse cowgirl position 103783
? sex 2358421
? short hair 1040961
? slightly open mouth 748
? solo focus 669417
? spread legs 913650
? straddling 97974
? straight 1057059
? sweat 1110318
? teeth 505048
? testicles 524973
? thick legs 145978
? thighhighs 695026
? tree 111361
? vaginal 227450
? vaginal creampie 2339
? vaginal penetration 1238663
? vaginal sex 243604
? veins 141011
? veiny penis 484172
? viewed from below 14750
? white thighhighs 35375
"""

# Split into lines and extract tags
tags_only = []
for line in huge_string.strip().splitlines():
    parts = line.lstrip("?").rsplit(" ", 1)
    if len(parts) == 2:
        tag, _ = parts
        tags_only.append(tag.strip())

# Output
for tag in tags_only:
    print(tag)
