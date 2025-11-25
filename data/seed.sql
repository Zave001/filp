-- 1. Категории с required_attributes

INSERT INTO categories (name, description, required_attributes) VALUES

('Электрогитары', 'Электрогитары и бас-гитары для разных стилей музыки',

'{"body_wood": "string", "neck_wood": "string", "scale_length": "float", "frets": "int", "pickup_config": "string", "bridge_type": "string"}'::JSONB),

('Акустические гитары', 'Акустические и классические гитары',

'{"body_wood": "string", "top_wood": "string", "back_wood": "string", "scale_length": "float", "nut_width": "float"}'::JSONB),

('Усилители', 'Гитарные усилители и кабинеты',

'{"power_w": "int", "amp_type": "string", "channels": "int", "speaker_size": "int", "has_reverb": "bool"}'::JSONB),

('Педали эффектов', 'Педали гитарных эффектов и процессоры',

'{"effect_type": "string", "true_bypass": "bool", "power_supply": "string", "is_stereo": "bool"}'::JSONB),

('Струны', 'Струны для гитар и других инструментов',

'{"gauge": "string", "material": "string", "set_type": "string", "quantity": "int"}'::JSONB),

('Аксессуары', 'Чехлы, ремни, медиаторы, каподастры',

'{"type": "string", "material": "string", "size": "string", "length": "float"}'::JSONB);

-- 2. Производители (исправлены ID для последовательности)

INSERT INTO manufacturers (name, country) VALUES

('Yamaha', 'Япония'),

('Epiphone', 'США'),

('Jackson', 'США'),

('ESP', 'Япония'),

('LTD', 'Япония'),

('PRS', 'США'),

('Fender', 'США'),

('Ibanez', 'Япония'),

('Cort', 'Южная Корея'),

('Taylor', 'США'),

('Martin', 'США'),

('Orange', 'Великобритания'),

('Blackstar', 'Великобритания'),

('Marshall', 'Великобритания'),

('Boss', 'Япония'),

('TC Electronic', 'Дания'),

('Mooer', 'Китай'),

('Jim Dunlop', 'США'),

('Elixir', 'США'),

('GHS', 'США'),

('DAddario', 'США'),

('Ernie Ball', 'США'),

('Planet Waves', 'США'),

('Vox', 'Великобритания');

-- 3. Товары с атрибутами в JSONB

INSERT INTO products (name, category_id, manufacturer_id, price, inStock, attributes) VALUES

-- Электрогитары (category_id = 1)

('Yamaha Pacifica 112V', 1, 1, 42990.00, true,

'{"body_wood": "Alder", "neck_wood": "Maple", "scale_length": 25.5, "frets": 22, "pickup_config": "HSS", "bridge_type": "Tremolo"}'::JSONB),

('Epiphone Les Paul Standard', 1, 2, 59990.00, true,

'{"body_wood": "Mahogany", "neck_wood": "Mahogany", "scale_length": 24.75, "frets": 22, "pickup_config": "HH", "bridge_type": "Tune-o-matic"}'::JSONB),

('Jackson JS22 Dinky', 1, 3, 32990.00, true,

'{"body_wood": "Basswood", "neck_wood": "Maple", "scale_length": 25.5, "frets": 24, "pickup_config": "H", "bridge_type": "Floyd Rose"}'::JSONB),

('ESP LTD EC-256', 1, 4, 67990.00, true,

'{"body_wood": "Mahogany", "neck_wood": "Mahogany", "scale_length": 24.75, "frets": 22, "pickup_config": "HH", "bridge_type": "Tune-o-matic"}'::JSONB),

('PRS SE Custom 24', 1, 6, 89990.00, true,

'{"body_wood": "Mahogany", "neck_wood": "Maple", "scale_length": 25.0, "frets": 24, "pickup_config": "HH", "bridge_type": "Tremolo"}'::JSONB),

('Ibanez RG550', 1, 8, 99990.00, true,

'{"body_wood": "Basswood", "neck_wood": "Maple", "scale_length": 25.5, "frets": 24, "pickup_config": "HH", "bridge_type": "Floyd Rose"}'::JSONB),

('Fender Squier Bullet Strat', 1, 7, 19990.00, true,

'{"body_wood": "Basswood", "neck_wood": "Maple", "scale_length": 25.5, "frets": 21, "pickup_config": "SSS", "bridge_type": "Tremolo"}'::JSONB),

('Cort CR250', 1, 9, 45990.00, true,

'{"body_wood": "Mahogany", "neck_wood": "Mahogany", "scale_length": 25.5, "frets": 24, "pickup_config": "HH", "bridge_type": "Fixed"}'::JSONB),

('Jackson Rhoads JS32', 1, 3, 41990.00, true,

'{"body_wood": "Basswood", "neck_wood": "Maple", "scale_length": 25.5, "frets": 24, "pickup_config": "HH", "bridge_type": "Floyd Rose"}'::JSONB),

('ESP LTD M-100', 1, 4, 52990.00, true,

'{"body_wood": "Mahogany", "neck_wood": "Mahogany", "scale_length": 24.75, "frets": 22, "pickup_config": "HH", "bridge_type": "Tune-o-matic"}'::JSONB),

-- Акустические гитары (category_id = 2)

('Yamaha F310', 2, 1, 15990.00, true,

'{"body_wood": "Meranti", "top_wood": "Spruce", "back_wood": "Meranti", "scale_length": 25.6, "nut_width": 1.69}'::JSONB),

('Taylor 114ce', 2, 10, 89990.00, true,

'{"body_wood": "Walnut", "top_wood": "Spruce", "back_wood": "Walnut", "scale_length": 25.5, "nut_width": 1.6875}'::JSONB),

('Martin DX1RAE', 2, 11, 75990.00, true,

'{"body_wood": "HPL", "top_wood": "Spruce pattern", "back_wood": "HPL", "scale_length": 25.4, "nut_width": 1.75}'::JSONB),

('Epiphone Hummingbird', 2, 2, 39990.00, true,

'{"body_wood": "Mahogany", "top_wood": "Spruce", "back_wood": "Mahogany", "scale_length": 24.75, "nut_width": 1.68}'::JSONB),

('Fender CD-60S', 2, 7, 22990.00, true,

'{"body_wood": "Mahogany", "top_wood": "Spruce", "back_wood": "Mahogany", "scale_length": 25.3, "nut_width": 1.69}'::JSONB),

-- Усилители (category_id = 3)

('Orange Crush 20', 3, 12, 19990.00, true,

'{"power_w": 20, "amp_type": "Transistor", "channels": 1, "speaker_size": 8, "has_reverb": false}'::JSONB),

('Blackstar ID Core 10', 3, 13, 12990.00, true,

'{"power_w": 10, "amp_type": "Digital", "channels": 6, "speaker_size": 6, "has_reverb": true}'::JSONB),

('Marshall DSL1CR', 3, 14, 37990.00, true,

'{"power_w": 1, "amp_type": "Tube", "channels": 2, "speaker_size": 8, "has_reverb": true}'::JSONB),

('Fender Champion 20', 3, 7, 14990.00, true,

'{"power_w": 20, "amp_type": "Transistor", "channels": 1, "speaker_size": 8, "has_reverb": true}'::JSONB),

('Orange Crush 35RT', 3, 12, 32990.00, true,

'{"power_w": 35, "amp_type": "Transistor", "channels": 2, "speaker_size": 10, "has_reverb": true}'::JSONB),

-- Педали эффектов (category_id = 4)

('Boss SD-1', 4, 15, 6990.00, true,

'{"effect_type": "Overdrive", "true_bypass": false, "power_supply": "9V Adapter", "is_stereo": false}'::JSONB),

('TC Electronic PolyTune 3', 4, 16, 8990.00, true,

'{"effect_type": "Tuner", "true_bypass": true, "power_supply": "Battery/Adapter", "is_stereo": false}'::JSONB),

('Mooer GE200', 4, 17, 22990.00, true,

'{"effect_type": "Multi-effects", "true_bypass": true, "power_supply": "Adapter", "is_stereo": true}'::JSONB),

('Boss CH-1', 4, 15, 8990.00, true,

'{"effect_type": "Chorus", "true_bypass": false, "power_supply": "9V Adapter", "is_stereo": false}'::JSONB),

-- Струны (category_id = 5)

('Elixir Nanoweb 10-46', 5, 19, 1890.00, true,

'{"gauge": "10-46", "material": "Nickel Plated Steel", "set_type": "Electric", "quantity": 6}'::JSONB),

('GHS Boomers 10-46', 5, 20, 990.00, true,

'{"gauge": "10-46", "material": "Nickel Plated Steel", "set_type": "Electric", "quantity": 6}'::JSONB),

('Ernie Ball Paradigm 10-46', 5, 22, 2190.00, true,

'{"gauge": "10-46", "material": "Reinforced Steel", "set_type": "Electric", "quantity": 6}'::JSONB),

('DAddario NYXL 10-46', 5, 21, 2390.00, true,

'{"gauge": "10-46", "material": "NYXL Alloy", "set_type": "Electric", "quantity": 6}'::JSONB),

-- Кабели и аксессуары (category_id = 6)

('DAddario кабель 6м', 6, 21, 2990.00, true,

'{"type": "Instrument Cable", "material": "Braided Shield", "length": 6.0, "connector_type": "1/4 TS"}'::JSONB),

('Jim Dunlop Tortex медиаторы (набор)', 6, 18, 790.00, true,

'{"type": "Picks", "material": "Tortex", "size": "0.73mm", "quantity": 12}'::JSONB),

('Ernie Ball ремень', 6, 22, 2490.00, true,

'{"type": "Strap", "material": "Polypropylene", "size": "Adjustable"}'::JSONB),

('Planet Waves каподастр', 6, 23, 1490.00, true,

'{"type": "Capo", "material": "Aluminum", "size": "Universal"}'::JSONB),

('Jim Dunlop тюнер PW-CT-12', 6, 18, 2990.00, true,

'{"type": "Clip-on Tuner", "material": "Plastic", "size": "Compact"}'::JSONB);

-- 4. Пользователи для тестирования

INSERT INTO users (userName, email) VALUES

('testuser', 'test@example.com'),

('guitarist', 'guitar@example.com'),

('bassplayer', 'bass@example.com');