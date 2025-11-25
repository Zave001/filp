import { useState, useEffect } from 'react';
import api from '../services/api';
import './Orders.css';

function Orders({ userId }) {
  const [orders, setOrders] = useState([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (userId) {
      loadOrders();
    }
  }, [userId]);

  const loadOrders = async () => {
    setLoading(true);
    try {
      const data = await api.getOrders(userId);
      setOrders(data);
    } catch (error) {
      console.error('Failed to load orders:', error);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return <div className="loading">Загрузка заказов...</div>;
  }

  return (
    <div className="orders-container">
      <h2>Мои заказы</h2>
      {orders.length === 0 ? (
        <div className="no-orders">У вас пока нет заказов</div>
      ) : (
        <div className="orders-list">
          {orders.map(order => (
            <div key={order.orderID} className="order-card">
              <div className="order-header">
                <span className="order-id">Заказ #{order.orderID}</span>
                <span className="order-date">{new Date(order.orderDate).toLocaleDateString()}</span>
              </div>
              <div className="order-details">
                <div className="order-items">
                  {order.items.map(([productId, quantity]) => (
                    <div key={productId} className="order-item">
                      Товар {productId}: {quantity} шт.
                    </div>
                  ))}
                </div>
                <div className="order-prices">
                  <div>Общая стоимость: {order.totalCost} ₽</div>
                  <div>Итоговая стоимость: {order.finalCost} ₽</div>
                </div>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default Orders;