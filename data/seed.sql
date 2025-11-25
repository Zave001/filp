-- Категории (точно как в отчёте)
INSERT INTO categories (name, description, required_attributes) VALUES
('Электрогитары', 'Электрогитары и бас-гитары', '{"body_wood": "string", "neck_wood": "string", "scale_length": "float", "frets": "int", "pickup_config": "string", "bridge_type": "string"}'::jsonb),
('Акустические гитары', 'Акустические и электроакустические гитары', '{"body_wood": "string", "top_wood": "string", "scale_length": "float"}'::jsonb),
('Усилители', 'Гитарные комбо и головы', '{"power_w": "int", "amp_type": "string", "channels": "int", "has_reverb": "bool"}'::jsonb),
('Струны', 'Струны для гитар', '{"gauge": "string", "material": "string"}'::jsonb),
('Кабели', 'Инструментальные кабели', '{"length": "float", "connector_type": "string"}'::jsonb);

-- Производители
INSERT INTO manufacturers (name, country) VALUES
('Yamaha', 'Япония'), ('Fender', 'США'), ('Epiphone', 'США'), ('Marshall', 'Великобритания'),
('Boss', 'Япония'), ('Elixir', 'США'), ('DAddario', 'США');

-- Товары (примеры из отчёта)
INSERT INTO products (name, category_id, manufacturer_id, price, in_stock, attributes) VALUES
('Yamaha Pacifica 112V', 1, 1, 42990.00, true,
 '{"body_wood": "Alder", "neck_wood": "Maple", "scale_length": 25.5, "frets": 22, "pickup_config": "HSS", "bridge_type": "Tremolo"}'::jsonb),
('Marshall DSL1CR', 3, 4, 37990.00, true,
 '{"power_w": 1, "amp_type": "Tube", "channels": 2, "has_reverb": true}'::jsonb),
('Elixir Nanoweb 10-46', 4, 6, 1890.00, true,
 '{"gauge": "10-46", "material": "Nickel Plated"}'::jsonb),
('DAddario кабель 6м', 5, 7, 2990.00, true,
 '{"length": 6.0, "connector_type": "1/4 TS"}'::jsonb);

-- Пользователь для тестов
INSERT INTO users (username, email) VALUES ('guitarist', 'guitar@example.com');