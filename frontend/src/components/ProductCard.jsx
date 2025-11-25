import './ProductCard.css';

function ProductCard({ product, onAddToCart }) {
  const handleAddToCart = () => {
    onAddToCart(product);
  };

  const formatPrice = (price) => {
    return new Intl.NumberFormat('ru-RU', {
      style: 'currency',
      currency: 'RUB'
    }).format(price);
  };

  const getAttributesList = (attributes) => {
    if (!attributes || typeof attributes !== 'object') return [];

    return Object.entries(attributes).map(([key, value]) => ({
      key: key.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase()),
      value: Array.isArray(value) ? value.join(', ') : String(value)
    }));
  };

  return (
    <div className="product-card">
      <div className="product-header">
        <h3 className="product-name">{product.name}</h3>
        <div className={`stock-status ${product.inStock ? 'in-stock' : 'out-of-stock'}`}>
          {product.inStock ? 'В наличии' : 'Нет в наличии'}
        </div>
      </div>

      <div className="product-price">
        {formatPrice(product.price)}
      </div>

      <div className="product-attributes">
        {getAttributesList(product.attributes).slice(0, 4).map((attr, index) => (
          <div key={index} className="attribute">
            <span className="attribute-key">{attr.key}:</span>
            <span className="attribute-value">{attr.value}</span>
          </div>
        ))}
      </div>

      <button
        className="add-to-cart-btn"
        onClick={handleAddToCart}
        disabled={!product.inStock}
      >
        {product.inStock ? 'Добавить в корзину' : 'Нет в наличии'}
      </button>
    </div>
  );
}

export default ProductCard;