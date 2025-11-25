import './Header.css';

function Header({ user, cartCount, onViewChange, onLogout }) {
  return (
    <header className="header">
      <div className="header-content">
        <h1 className="logo" onClick={() => onViewChange('products')}>
          MusicalHub
        </h1>

        <nav className="nav">
          <button
            className="nav-button"
            onClick={() => onViewChange('products')}
          >
            Товары
          </button>

          {user ? (
            <>
              <button
                className="nav-button cart-button"
                onClick={() => onViewChange('cart')}
              >
                Корзина ({cartCount})
              </button>
              <span className="user-info">
                Привет, {user.userName}!
              </span>
              <button
                className="nav-button logout-button"
                onClick={onLogout}
              >
                Выйти
              </button>
            </>
          ) : (
            <button
              className="nav-button auth-button"
              onClick={() => onViewChange('auth')}
            >
              Войти
            </button>
          )}
        </nav>
      </div>
    </header>
  );
}

export default Header;