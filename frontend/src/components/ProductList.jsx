import { useState, useEffect } from 'react';
import api from '../services/api';
import ProductCard from './ProductCard';
import './ProductList.css';

function ProductList({ onAddToCart }) {
  const [filteredProducts, setFilteredProducts] = useState([]);
  const [loading, setLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');
  const [selectedCategory, setSelectedCategory] = useState('');
  const [selectedManufacturer, setSelectedManufacturer] = useState('');
  const [inStockOnly, setInStockOnly] = useState(false);
  const [sortBy, setSortBy] = useState('');

  useEffect(() => {
    loadFilteredProducts();
  }, [searchQuery, selectedCategory, selectedManufacturer, inStockOnly, sortBy]);

  const loadFilteredProducts = async () => {
    setLoading(true);
    try {
      const params = {};
      if (searchQuery) params.q = searchQuery;
      if (selectedCategory) params.category = selectedCategory;
      if (selectedManufacturer) params.manufacturer = selectedManufacturer;
      if (inStockOnly) params.inStock = true;
      if (sortBy) params.sort = sortBy;

      const data = await api.getFilteredProducts(params);
      setFilteredProducts(data);
    } catch (error) {
      console.error('Failed to load products:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = (e) => {
    e.preventDefault();
    applyFilters();
  };

  const clearFilters = () => {
    setSearchQuery('');
    setSelectedCategory('');
    setSelectedManufacturer('');
    setInStockOnly(false);
    setSortBy('');
    setFilteredProducts(products);
  };

  if (loading) {
    return <div className="loading">Загрузка товаров...</div>;
  }

  return (
    <div className="product-list-container">
      <div className="filters-section">
        <form onSubmit={handleSearch} className="search-form">
          <input
            type="text"
            placeholder="Поиск товаров..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="search-input"
          />
          <button type="submit" className="search-button">Поиск</button>
        </form>

        <div className="filter-controls">
          <select
            value={selectedCategory}
            onChange={(e) => setSelectedCategory(e.target.value)}
            className="filter-select"
          >
            <option value="">Все категории</option>
            <option value="1">Электрогитары</option>
            <option value="2">Акустические гитары</option>
            <option value="3">Усилители</option>
            <option value="4">Педали эффектов</option>
            <option value="5">Струны</option>
            <option value="6">Аксессуары</option>
          </select>

          <select
            value={selectedManufacturer}
            onChange={(e) => setSelectedManufacturer(e.target.value)}
            className="filter-select"
          >
            <option value="">Все производители</option>
            <option value="1">Yamaha</option>
            <option value="2">Epiphone</option>
            <option value="3">Jackson</option>
            <option value="4">ESP</option>
            <option value="5">PRS</option>
            <option value="6">Fender</option>
            <option value="7">Ibanez</option>
            <option value="8">Marshall</option>
            <option value="9">Boss</option>
          </select>

          <select
            value={sortBy}
            onChange={(e) => setSortBy(e.target.value)}
            className="filter-select"
          >
            <option value="">Без сортировки</option>
            <option value="price-asc">Цена: по возрастанию</option>
            <option value="price-desc">Цена: по убыванию</option>
            <option value="name">По названию</option>
          </select>

          <label className="stock-checkbox">
            <input
              type="checkbox"
              checked={inStockOnly}
              onChange={(e) => setInStockOnly(e.target.checked)}
            />
            Только в наличии
          </label>

          <button type="button" onClick={clearFilters} className="clear-button">
            Сбросить фильтры
          </button>
        </div>
      </div>

      <div className="products-grid">
        {filteredProducts.length === 0 ? (
          <div className="no-products">Товары не найдены</div>
        ) : (
          filteredProducts.map(product => (
            <ProductCard
              key={product.productID}
              product={product}
              onAddToCart={onAddToCart}
            />
          ))
        )}
      </div>
    </div>
  );
}

export default ProductList;