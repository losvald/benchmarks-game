/*
 * pidigits.cpp
 *
 *  Created on: Oct 2, 2011
 *      Author: Leo Osvald
 */

#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>
#include <string>

template<typename T>
T Log10(const T& x) {
  static const T kLog10 = log(10.);
  return log(x) / kLog10;
}

typedef std::vector<int> Magnitude;

/**
 * C++ translation of R implementation of big integer.
 * (without usage of long long or bit operations
 * since these don't exist in R)
 */
class BigInt {
public:
  static const int kElemMax = 10000;
  static const std::size_t kElemDigits = 4;
  static const BigInt kTen;
  static const BigInt kZero;
  static const BigInt kOne;
private:
  Magnitude m;
  int sign;

public:
  BigInt(std::string s) {
    if (s[0] == '-') {
      sign = -1;
      s.erase(0, 1);
    } else
      sign = 1;

    StripLeadingZeros(&s);
    std::size_t len = s.length();
    if (len == 0) {
      m = kZero.m;
      sign = 0;
      return ;
    }

    std::size_t mod = len % kElemDigits;
    if (mod)
      m.push_back(atoi(s.substr(0, mod).c_str()));
    for (std::size_t i = mod; i < len; i += kElemDigits) {
      m.push_back(atoi(s.substr(i, kElemDigits).c_str()));
    }
  }

  BigInt(int x) {
    if ((sign = CmpElem(x, 0)) < 0)
      x = -x;
    for (; x != 0; x /= kElemMax)
      m.push_back(x % kElemMax);
    std::reverse(m.begin(), m.end());
  }

  BigInt() : sign(0) { }

  operator int() const {
    int ret = 0;
    for (std::size_t i = m.size(); i > 0; )
      ret = ret * kElemMax + m[--i];
    return sign * ret;
  }

  friend bool operator==(const BigInt& x, const BigInt& y) {
    return x.Cmp(y) == 0;
  }
  friend bool operator!=(const BigInt& x, const BigInt& y) {
    return x.Cmp(y) != 0;
  }
  friend bool operator<=(const BigInt& x, const BigInt& y) {
    return x.Cmp(y) <= 0;
  }
  friend bool operator<(const BigInt& x, const BigInt& y) {
    return x.Cmp(y) < 0;
  }
  friend bool operator>=(const BigInt& x, const BigInt& y) {
    return x.Cmp(y) >= 0;
  }
  friend bool operator>(const BigInt& x, const BigInt& y) {
    return x.Cmp(y) > 0;
  }

  friend BigInt operator+(const BigInt& x, const BigInt& y) {
    if (y.sign == 0)
      return x;
    if (x.sign == 0)
      return y;
    if (x.sign == y.sign)
      return BigInt(x.sign, AddMag(x.m, y.m));

    int c = CmpMag(x.m, y.m);
    if (c == 0)
      return kZero;
    Magnitude result_mag = (c > 0 ? SubMag(x.m, y.m) : SubMag(y.m, x.m));
    StripLeadingZeroElems(&result_mag);
    return BigInt(SignProd(c, x.sign), result_mag);
  }

  friend BigInt operator-(const BigInt& x, const BigInt& y) {
    if (y.sign == 0)
      return x;
    if (x.sign == 0)
      return -y;
    if (x.sign != y.sign)
      return BigInt(x.sign, AddMag(x.m, y.m));

    int c = CmpMag(x.m, y.m);
    if (c == 0)
      return kZero;
    Magnitude result_mag = (c > 0 ? SubMag(x.m, y.m) : SubMag(y.m, x.m));
    StripLeadingZeroElems(&result_mag);
    return BigInt(SignProd(c, x.sign), result_mag);
  }

  friend BigInt operator-(const BigInt& x) {
    return BigInt(-x.sign, x.m);
  }

  friend BigInt operator*(const BigInt& x, const BigInt& y) {
    if (y.sign == 0 || x.sign == 0)
      return kZero;

    Magnitude result_mag = MulMag(x.m, y.m);
    StripLeadingZeroElems(&result_mag);
    return BigInt(SignProd(x.sign, y.sign), result_mag);
  }

  friend BigInt operator/(const BigInt& x, const BigInt& y) {
    if (y.sign == 0)
      throw "Division by zero";
    if (x.sign == 0)
      return kZero;
    int c = CmpMag(x.m, y.m);
    if (c == 0)
      return kOne;
    if (c < 0)
      return kZero;
    return BigInt(SignProd(x.sign, y.sign), DivMag(x.m, y.m));
  }

  friend BigInt operator%(const BigInt& x, const BigInt& y) {
    return x - x / y * y;
  }

  friend std::ostream& operator<<(std::ostream& os, const BigInt& x) {
    if (x.sign == 0)
      os << "0";
    else {
      if (x.sign < 0)
        os << "-";
      os << x.m[0];
      for (std::size_t i = 1; i < x.m.size(); ++i) {
        os.fill('0'); os.width(kElemDigits);
        os << x.m[i];
      }
    }
    return os;
  }

  int Cmp(const BigInt& y) const {
    if (sign == y.sign) {
      if (sign == 1) return CmpMag(m, y.m);
      if (sign == -1) return CmpMag(y.m, m);
      return 0;
    }
    return (sign > y.sign) - (sign < y.sign);
  }

  BigInt Div2() const {
    if (sign == 0 || (m.size() == 1 && m[0] == 1L))
      return kZero;
    return BigInt(sign, Div2Mag(m));
  }

private:
  BigInt(int sign, const Magnitude& magnitude) : m(magnitude), sign(sign) { }

  static void StripLeadingZeros(std::string* s) {
    std::size_t ind = s->find_first_not_of("0");
    if (ind == std::string::npos)
      s->clear();
    else
      s->erase(s->begin(), s->begin() + ind);
  }

  static inline int CmpElem(int x, int y) {
    return (x > y) - (x < y);
  }

  static inline int SignProd(int x, int y) {
    return (x == y) - (x != y);
  }

  static int CmpMag(const Magnitude& x, const Magnitude& y) {
    std::size_t x_len = x.size();
    std::size_t y_len = y.size();
    if (x_len < y_len) return -1;
    if (x_len > y_len) return 1;
    for (std::size_t i = 0; i < x_len; ++i)
      if (int c = CmpElem(x[i], y[i]))
        return c;
    return 0;
  }

  static Magnitude AddMag(const Magnitude& tx, const Magnitude& ty) {
    const Magnitude& x = (tx.size() > ty.size() ? tx : ty);
    const Magnitude& y = (tx.size() > ty.size() ? ty : tx);

    std::size_t x_index = x.size();
    std::size_t y_index = y.size();
    Magnitude result(x.size());
    int sum = 0;

    // add common parts of both numbers
    while (y_index > 0U) {
      sum = x[--x_index] + y[--y_index] + sum / kElemMax;
      result[x_index] = sum % kElemMax;
    }

    // Copy remainder of longer number while carry propagation is required
    bool carry = (sum >= kElemMax);
    while (x_index > 0U && carry) {
      --x_index;
      carry = ((result[x_index] = x[x_index] + 1) == 0);
    }

    // Copy remainder of longer number
    while (x_index > 0U) {
      --x_index;
      result[x_index] = x[x_index];
    }

    // Grow result if necessary
    if (carry) {
      Magnitude bigger(result.size() + 1);
      std::copy(result.begin(), result.end(), bigger.begin() + 1);
      bigger[0] = 0x01;
      bigger[0] = 0x01;
      return bigger;
    }
    return result;
  }

  static Magnitude SubMag(const Magnitude& big, const Magnitude& little) {
    std::size_t big_index = big.size();
    Magnitude result(big_index);
    std::size_t little_index = little.size();
    int difference = 0;

    // Subtract common parts of both numbers
    while(little_index > 0) {
      difference = big[--big_index] - little[--little_index] +
          (difference < 0 ? -1 : 0);
      result[big_index] = (difference + kElemMax) % kElemMax;
    }

    // Subtract remainder of longer number while borrow propagates
    bool borrow = (difference < 0 ? -1 : 0);
    while (big_index > 0U && borrow) {
      --big_index;
      borrow = ((result[big_index] = big[big_index] - 1) == -1);
    }

    // Copy remainder of longer number
    while (big_index > 0U) {
      --big_index;
      result[big_index] = big[big_index];
    }

    return result;
  }

  static Magnitude MulMag(const Magnitude& x, const Magnitude& y) {
    int x_start = x.size() - 1;
    int y_start = y.size() - 1;
    Magnitude c(x.size() + y.size());

    long carry = 0;
    for (int j = y_start, k = y_start + 1 + x_start; j >= 0; j--, k--) {
      long product = y[j] * x[x_start] + carry;
      c[k] = product % kElemMax;
      carry = product / kElemMax;
    }
    c[x_start] = carry;

    for (int i = x_start - 1; i >= 0; i--) {
      carry = 0;
      for (int j = y_start, k = y_start + 1 + i; j >= 0; j--, k--) {
        long product = y[j] * x[i] + c[k] + carry;
        c[k] = product % kElemMax;
        carry = product / kElemMax;
      }
      c[i] = carry % kElemMax;
    }
    return c;
  }

  static Magnitude DivMag(const Magnitude& x_mag, const Magnitude& y_mag) {
    if (y_mag == kOne.m)
      return x_mag;

    BigInt x(1, x_mag), y(1, y_mag);
    const std::size_t x_mag_log10 = Log10Mag(x_mag);
    const std::size_t y_mag_log10 = Log10Mag(y_mag);
    const std::size_t lo_log10 = x_mag_log10 - y_mag_log10 -
      (x_mag_log10 != y_mag_log10);
    const std::size_t hi_log10 = x_mag_log10 - y_mag_log10 + 1;
    BigInt lo = Pow10(lo_log10), hi;

    // try pruning hi > 10 or lo <= 10
    if (lo_log10 == 0 && hi_log10 > 1) {
      const BigInt lo10 = lo * kTen;
      if (lo10 * y > x)
	hi = lo10;
      else {
	lo = lo10;
	hi = Pow10(hi_log10);
      }
    } else
      hi = Pow10(hi_log10);

    while (lo < hi) {
      BigInt mid = (lo + hi + kOne).Div2();
      if (mid * y <= x)
	lo = mid;
      else
	hi = mid - kOne;
    }
    return lo.m;
  }

  static Magnitude Div2Mag(const Magnitude& x) {
    if (x.size() == 1)
      return Magnitude(1, x[0] / 2);

    int borrow = (x[0] == 1);
    std::size_t x_start = borrow, x_end = x.size();
    std::size_t result_index = 0;
    Magnitude result(x_end - x_start);
    for (std::size_t x_index = x_start; x_index < x_end; ++x_index) {
      int d = x[x_index] + kElemMax * borrow;
      result[result_index++] = d / 2;
      borrow = d % 2;
    }
    return result;
  }

  static inline std::size_t Log10Mag(const Magnitude& m) {
    return kElemDigits * (m.size() - 1U) + 
      (std::size_t)Log10<double>(m[0]);
  }

  static inline BigInt Pow10(std::size_t n) {
    std::size_t n_div = n / kElemDigits;
    std::size_t n_mod = n - n_div * kElemDigits;
    Magnitude m(1 + n_div, 0);
    for (m[0] = 1; n_mod--; )
      m[0] *= 10;
    return BigInt(1, m);
  }

  static void StripLeadingZeroElems(Magnitude* m) {
    for (std::size_t i = 0; i < m->size(); ++i)
      if ((*m)[i]) {
        m->erase(m->begin(), m->begin() + i);
        return;
      }
    m->clear();
  }

};

const BigInt BigInt::kZero(0L, Magnitude(0));
const BigInt BigInt::kOne(1L, Magnitude(1, 1));
const BigInt BigInt::kTen("10");


// pidigits program

void PiDigits(int limit) {
  int i, k, ns, k1;

  BigInt n, a, d, t, u;
  const BigInt kTwo = BigInt::kOne + BigInt::kOne;
  const BigInt kThree = kTwo + BigInt::kOne;

  i = k = ns = 0;
  k1 = 1;
  n = d = BigInt::kOne;
  a = t = u = BigInt::kZero;
  while (true) {
    ++k;
    t = n * kTwo;
    n = n * BigInt(k);
    BigInt k2(k1 += 2);
    a = (a + t) * k2;
    d = d * k2;
    if (a >= n) {
      BigInt three_n_plus_a = n * kThree + a;
      t = three_n_plus_a / d;
      // u = three_n_plus_a % d + n;
      BigInt td = t * d;
      u = three_n_plus_a - td + n;
      if (d > u) {
        ns = ns * 10 + (int)t;
        if (++i % 5 == 0) {
          std::cout.fill('0');
          std::cout.width(5);
          std::cout << ns;
          if (i % 2 == 0)
            std::cout << "\t:" << i << std::endl;
          ns = 0;
        }
        if (i >= limit)
          break;
        a = (a - td) * BigInt::kTen;  // TODO use precomputed d * t
        n = n * BigInt::kTen;
      }
    }
  }
}

int main(int argc, char **argv) {
  int n = atoi(argv[1]); 
  PiDigits(n);
  return 0;
}

