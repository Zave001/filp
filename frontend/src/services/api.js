const API_BASE = 'http://localhost:8080/api';

class ApiService {
  async request(endpoint, options = {}) {
    const url = `${API_BASE}${endpoint}`;
    const config = {
      headers: {
        'Content-Type': 'application/json',
        ...options.headers,
      },
      ...options,
    };

    try {
      const response = await fetch(url, config);
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return await response.json();
    } catch (error) {
      console.error('API request failed:', error);
      throw error;
    }
  }

  // Auth
  async register(userData) {
    return this.request('/auth/register', {
      method: 'POST',
      body: JSON.stringify(userData),
    });
  }

  async login(credentials) {
    return this.request('/auth/login', {
      method: 'POST',
      body: JSON.stringify(credentials),
    });
  }

  // Products
  async getProducts() {
    return this.request('/products');
  }

  async getFilteredProducts(params = {}) {
    const query = new URLSearchParams(params).toString();
    return this.request(`/products/filtered?${query}`);
  }

  async searchProducts(query) {
    return this.request(`/products/search?q=${encodeURIComponent(query)}`);
  }

  async getProductsByCategory(categoryId) {
    return this.request(`/products/category/${categoryId}`);
  }

  async getProductsByManufacturer(manufacturerId) {
    return this.request(`/products/manufacturer/${manufacturerId}`);
  }

  async getProductsByStock(inStock) {
    return this.request(`/products/stock/${inStock}`);
  }

  async sortProductsByPrice(order) {
    return this.request(`/products/sort/price/${order}`);
  }

  async sortProductsByName() {
    return this.request('/products/sort/name');
  }

  async filterByAttribute(attr, value) {
    return this.request(`/products/filter/attribute?attr=${attr}&value=${value}`);
  }

  async filterByNumeric(attr, value) {
    return this.request(`/products/filter/numeric?attr=${attr}&value=${value}`);
  }

  // Cart
  async calculateCart(cartData) {
    return this.request('/cart/calculate', {
      method: 'POST',
      body: JSON.stringify(cartData),
    });
  }

  // Orders
  async createOrder(orderData) {
    return this.request('/orders', {
      method: 'POST',
      body: JSON.stringify(orderData),
    });
  }

  async getOrders(userId) {
    return this.request(`/orders/${userId}`);
  }

  async getAllOrders() {
    return this.request('/orders');
  }
}

export default new ApiService();