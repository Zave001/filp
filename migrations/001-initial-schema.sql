-- Полная схема базы данных — 100% как в курсовой
DROP TABLE IF EXISTS orders, products, users, manufacturers, categories CASCADE;

CREATE TABLE categories (
    id SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    description TEXT,
    required_attributes JSONB NOT NULL DEFAULT '{}'::jsonb
);

CREATE TABLE manufacturers (
    id SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    country VARCHAR(50)
);

CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    category_id INTEGER REFERENCES categories(id),
    manufacturer_id INTEGER REFERENCES manufacturers(id),
    price DECIMAL(12,2) NOT NULL CHECK (price >= 1.0 AND price <= 5000000.0),
    in_stock BOOLEAN DEFAULT true,
    attributes JSONB NOT NULL DEFAULT '{}'::jsonb
);

CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE
        CHECK (LENGTH(username) >= 1 AND LENGTH(username) <= 50),
    email VARCHAR(100) NOT NULL UNIQUE
        CHECK (email ~ '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$')
);

CREATE TABLE orders (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(id),
    items JSONB NOT NULL,
    order_date TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    total_cost DECIMAL(12,2) NOT NULL CHECK (total_cost >= 1.0),
    final_cost DECIMAL(12,2) NOT NULL CHECK (final_cost >= 0.0 AND final_cost <= total_cost)
);