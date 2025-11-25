-- Полная схема базы данных — 100% как в курсовой
DROP TABLE IF EXISTS orders, products, users, manufacturers, categories CASCADE;

CREATE TABLE categories (
    CategoryID SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    description TEXT,
    required_attributes JSONB NOT NULL DEFAULT '{}'::JSONB
);

CREATE TABLE manufacturers (
    ManufacturerID SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    country VARCHAR(50)
);

CREATE TABLE products (
    productID SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    category_id INTEGER REFERENCES categories(CategoryID),
    manufacturer_id INTEGER REFERENCES manufacturers(ManufacturerID),
    price DECIMAL(12,2) NOT NULL CHECK (price >= 1.0 AND price <= 5000000.0),
    inStock BOOLEAN DEFAULT true,
    attributes JSONB NOT NULL DEFAULT '{}'::JSONB,
    CHECK (productID BETWEEN 1 AND 10000)
);

CREATE TABLE users (
    userID SERIAL PRIMARY KEY,
    userName VARCHAR(50) NOT NULL UNIQUE
        CHECK (LENGTH(userName) >= 1 AND LENGTH(userName) <= 50),
    email VARCHAR(100) NOT NULL UNIQUE
        CHECK (LENGTH(email) >= 3 AND LENGTH(email) <= 100 AND email ~ '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$'),
    password VARCHAR(255) NOT NULL,
    CHECK (userID BETWEEN 1 AND 10000)
);

CREATE TABLE orders (
    orderID SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(userID),
    items JSONB NOT NULL,
    orderDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    totalCost DECIMAL(12,2) NOT NULL
        CHECK (totalCost >= 1.0 AND totalCost <= 500000000.0),
    finalCost DECIMAL(12,2) NOT NULL
        CHECK (finalCost >= 0.0 AND finalCost <= 500000000.0),
    CHECK (orderID BETWEEN 1 AND 10000)
);