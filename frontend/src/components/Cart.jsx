import { useState, useEffect } from 'react';
import api from '../services/api';
import './Cart.css';

function Cart({ cart, user, onUpdateQuantity, onRemoveItem, onViewChange }) {
  const [loading, setLoading] = useState(false);
  const [orderPlaced, setOrderPlaced] = useState(false);
  const [calculatedTotals, setCalculatedTotals] = useState({ totalCost: 0, finalCost: 0, saved: 0, discountDescription: '' });

  useEffect(() => {
    if (cart.length > 0 && user) {
      calculateCartTotals();
    } else {
      setCalculatedTotals({ totalCost: 0, finalCost: 0, saved: 0, discountDescription: '' });
    }
  }, [cart, user]);

  const calculateCartTotals = async () => {
    try {
      const cartData = {
        userId: user.userId,
        items: cart.map(item => ({
          productId: item.productId,
          quantity: item.quantity
        }))
      };
      const result = await api.calculateCart(cartData);
      setCalculatedTotals(result);
    } catch (error) {
      console.error('Failed to calculate cart:', error);
      // Fallback to simple calculation
      const total = cart.reduce((sum, item) => sum + (item.price * item.quantity), 0);
      setCalculatedTotals({ totalCost: total, finalCost: total, saved: 0, discountDescription: 'Без скидки' });
    }
  };

  const calculateTotal = () => {
    return cart.reduce((total, item) => total + (item.price * item.quantity), 0);
  };

  const calculateDiscountedTotal = () => {
    return calculatedTotals.finalCost || calculateTotal();
  };

  const formatPrice = (price) => {
    return new Intl.NumberFormat('ru-RU', {
      style: 'currency',
      currency: 'RUB'
    }).format(price);
  };

  const handleQuantityChange = (productId, newQuantity) => {
    onUpdateQuantity(productId, parseInt(newQuantity) || 0);
  };

  const handlePlaceOrder = async () => {
    if (!user) {
      alert('Необходимо войти в систему для оформления заказа');
      onViewChange('auth');
      return;
    }

    if (cart.length === 0) {
      alert('Корзина пуста');
      return;
    }

    setLoading(true);
    try {
      const orderData = {
        userId: user.userId,
        items: cart.map(item => ({
          productId: item.productId,
          quantity: item.quantity
        }))
      };

      const result = await api.createOrder(orderData);

      if (result.orderId) {
        setOrderPlaced(true);
        // Clear cart after successful order
        cart.forEach(item => onRemoveItem(item.productId));
      }
    } catch (error) {
      console.error('Failed to place order:', error);
      alert('Ошибка при оформлении заказа. Попробуйте позже.');
    } finally {
      setLoading(false);
    }
  };

  if (orderPlaced) {
    return (
      <div className="cart-container">
        <div className="order-success">
          <h2>Заказ успешно оформлен!</h2>
          <p>Спасибо за покупку. Мы свяжемся с вами для подтверждения деталей.</p>
          <button
            className="continue-shopping-btn"
            onClick={() => {
              setOrderPlaced(false);
              onViewChange('products');
            }}
          >
            Продолжить покупки
          </button>
        </div>
      </div>
    );
  }

  if (cart.length === 0) {
    return (
      <div className="cart-container">
        <div className="empty-cart">
          <h2>Корзина пуста</h2>
          <p>Добавьте товары в корзину, чтобы оформить заказ.</p>
          <button
            className="continue-shopping-btn"
            onClick={() => onViewChange('products')}
          >
            Перейти к товарам
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="cart-container">
      <h2>Корзина</h2>

      <div className="cart-items">
        {cart.map(item => (
          <div key={item.productId} className="cart-item">
            <div className="item-info">
              <h3>{item.name}</h3>
              <p className="item-price">{formatPrice(item.price)}</p>
            </div>

            <div className="quantity-controls">
              <button
                className="quantity-btn"
                onClick={() => handleQuantityChange(item.productId, item.quantity - 1)}
              >
                -
              </button>
              <input
                type="number"
                min="1"
                value={item.quantity}
                onChange={(e) => handleQuantityChange(item.productId, e.target.value)}
                className="quantity-input"
              />
              <button
                className="quantity-btn"
                onClick={() => handleQuantityChange(item.productId, item.quantity + 1)}
              >
                +
              </button>
            </div>

            <div className="item-total">
              {formatPrice(item.price * item.quantity)}
            </div>

            <button
              className="remove-btn"
              onClick={() => onRemoveItem(item.productId)}
            >
              ✕
            </button>
          </div>
        ))}
      </div>

      <div className="cart-summary">
        <div className="summary-row">
          <span>Итого:</span>
          <span className="total-amount">{formatPrice(calculatedTotals.totalCost || calculateTotal())}</span>
        </div>

        {calculatedTotals.saved > 0 && (
          <>
            <div className="summary-row">
              <span>Скидка:</span>
              <span className="discount-amount">-{formatPrice(calculatedTotals.saved)}</span>
            </div>
            <div className="discount-description">
              {calculatedTotals.discountDescription}
            </div>
          </>
        )}

        <div className="summary-row">
          <span>Итого со скидками:</span>
          <span className="discounted-amount">{formatPrice(calculateDiscountedTotal())}</span>
        </div>
      </div>

      <div className="cart-actions">
        {!user && (
          <p className="login-prompt">
            Для оформления заказа необходимо <button
              className="login-link"
              onClick={() => onViewChange('auth')}
            >
              войти в систему
            </button>
          </p>
        )}

        <button
          className="place-order-btn"
          onClick={handlePlaceOrder}
          disabled={loading || !user}
        >
          {loading ? 'Оформление...' : 'Оформить заказ'}
        </button>
      </div>
    </div>
  );
}

export default Cart;