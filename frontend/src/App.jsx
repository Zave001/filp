import { useState, useEffect } from 'react';
import Header from './components/Header';
import ProductList from './components/ProductList';
import Auth from './components/Auth';
import Cart from './components/Cart';
import Orders from './components/Orders';
import './App.css';

function App() {
  const [currentView, setCurrentView] = useState('products');
  const [user, setUser] = useState(null);
  const [cart, setCart] = useState([]);

  useEffect(() => {
    // Check if user is logged in (from localStorage)
    const savedUser = localStorage.getItem('user');
    if (savedUser) {
      setUser(JSON.parse(savedUser));
    }
  }, []);

  const handleLogin = (userData) => {
    setUser(userData);
    localStorage.setItem('user', JSON.stringify(userData));
  };

  const handleLogout = () => {
    setUser(null);
    localStorage.removeItem('user');
    setCart([]);
  };

  const addToCart = (product, quantity = 1) => {
    setCart(prevCart => {
      const existing = prevCart.find(item => item.productId === product.productID);
      if (existing) {
        return prevCart.map(item =>
          item.productId === product.productID
            ? { ...item, quantity: item.quantity + quantity }
            : item
        );
      }
      return [...prevCart, {
        productId: product.productID,
        name: product.name,
        price: product.price,
        quantity
      }];
    });
  };

  const removeFromCart = (productId) => {
    setCart(prevCart => prevCart.filter(item => item.productId !== productId));
  };

  const updateCartQuantity = (productId, quantity) => {
    if (quantity <= 0) {
      removeFromCart(productId);
      return;
    }
    setCart(prevCart =>
      prevCart.map(item =>
        item.productId === productId ? { ...item, quantity } : item
      )
    );
  };

  const renderView = () => {
    switch (currentView) {
      case 'auth':
        return <Auth onLogin={handleLogin} onViewChange={setCurrentView} />;
      case 'cart':
        return (
          <Cart
            cart={cart}
            user={user}
            onUpdateQuantity={updateCartQuantity}
            onRemoveItem={removeFromCart}
            onViewChange={setCurrentView}
          />
        );
      case 'orders':
        return user ? <Orders userId={user.userId} /> : <div>Please log in to view orders</div>;
      case 'products':
      default:
        return <ProductList onAddToCart={addToCart} />;
    }
  };

  return (
    <div className="App">
      <Header
        user={user}
        cartCount={cart.reduce((sum, item) => sum + item.quantity, 0)}
        onViewChange={setCurrentView}
        onLogout={handleLogout}
      />
      <main className="main-content">
        {renderView()}
      </main>
    </div>
  );
}

export default App;
