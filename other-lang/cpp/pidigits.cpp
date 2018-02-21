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

typedef std::vector<int> Magnitude;

/**
 * C++ translation of R implementation of big integer.
 * (without usage of long long or bit operations
 * since these don't exist in R)
 */
class BigInt {
public:
  static const std::size_t kElemDigits = 14;
  static const int kElemMax = 1 << kElemDigits;  // base
  static const BigInt kTen;
  static const BigInt kZero;
  static const BigInt kOne;
private:
  Magnitude m;
  int sign;

public:
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

    return MagDiff(x, y);
  }

  friend BigInt operator-(const BigInt& x, const BigInt& y) {
    if (y.sign == 0)
      return x;
    if (x.sign == 0)
      return -y;
    if (x.sign != y.sign)
      return BigInt(x.sign, AddMag(x.m, y.m));
    return MagDiff(x, y);
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

  int Cmp(const BigInt& y) const {
    if (sign == 0) return 0;
    if (sign == y.sign) return sign * CmpMag(m, y.m);
    return (sign > y.sign) - (sign < y.sign);
  }

  BigInt Div2() const {
    return BigInt(sign, Div2Mag(m));
  }

private:
  BigInt(int sign, const Magnitude& magnitude) : m(magnitude), sign(sign) { }

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

  static BigInt MagDiff(const BigInt& x, const BigInt& y) {
    int c = CmpMag(x.m, y.m);
    if (c == 0)
      return kZero;
    Magnitude result_mag = (c > 0 ? SubMag(x.m, y.m) : SubMag(y.m, x.m));
    StripLeadingZeroElems(&result_mag);
    return BigInt(SignProd(c, x.sign), result_mag);
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
    c.back() = 0;
    for (int i = x_start; i >= 0; i--) {
      long carry = 0;
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
    BigInt x(1, x_mag), y(1, y_mag);
    const std::size_t x_mag_logBase = LogBaseMag(x_mag);
    const std::size_t y_mag_logBase = LogBaseMag(y_mag);
    const std::size_t lo_logBase = x_mag_logBase - y_mag_logBase -
      (x_mag_logBase != y_mag_logBase);
    const std::size_t hi_logBase = x_mag_logBase - y_mag_logBase + 1;
    BigInt lo = PowBase(lo_logBase), hi;

    // try pruning hi > base or lo <= base
    if (lo_logBase == 0 && hi_logBase > 1) {
      static BigInt kBase(kElemMax);
      const BigInt loBase = lo * kBase;
      if (loBase * y > x) {
        hi = loBase;
      }
      else {
        lo = loBase;
        hi = PowBase(hi_logBase);
      }
    } else
      hi = PowBase(hi_logBase);

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

  static inline std::size_t LogBaseMag(const Magnitude& m) {
    return m.size() - 1U;
  }

  static inline BigInt PowBase(std::size_t n) {
    Magnitude m(1 + n, 0);
    m[0] = 1;
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
const BigInt BigInt::kTen(10);


// pidigits program

void PiDigits(int limit) {
  const auto& kZero = BigInt::kZero;
  const auto& kOne = BigInt::kOne;
  const auto& kTen = BigInt::kTen;
  const BigInt kTwo = BigInt::kOne + BigInt::kOne;
  const BigInt kThree = kTwo + BigInt::kOne;

  int k, ns;
  k = ns = 0;

  auto rec0 = [&](const auto& rec, int i0,
                  const BigInt& n_div_k, const BigInt& d0,
                  const BigInt& a0) -> void {
    if (i0 < limit) {
      ++k;
      withMul(n_div_k, kTwo, [&](const auto& t0) {
        withMul(n_div_k, BigInt(k), [&](const auto& n) {
          withBigInt(2*k + 1, [&](const auto& k2) {
            withAdd(a0, t0, [&](const auto& a0t0sum) {
              withMul(a0t0sum, k2, [&](const auto& a) {
                withMul(d0, k2, [&](const auto& d) {
                  if (a >= n) {
                    withMul(n, kThree, [&](const auto& nMul3) {
                      withAdd(nMul3, a, [&](const auto& three_n_plus_a) {
                        withDiv(three_n_plus_a, d, [&](const auto& t) {
                          // u = three_n_plus_a % d + n;
                          withMul(t, d, [&](const auto& td) {
                            withSub(three_n_plus_a, td, [&](const auto& u_n) {
                              withAdd(u_n, n, [&](const auto& u) {
                                if (d > u) {
                                  ns = ns * 10 + (int)t;
                                  int i = i0 + 1;
                                  if (i % 5 == 0) {
                                    std::cout.fill('0');
                                    std::cout.width(5);
                                    std::cout << ns;
                                    if (i % 2 == 0)
                                      std::cout << "\t:" << i << std::endl;
                                    ns = 0;
                                  }
                                  withSub(a, td, [&](const auto& a_td) {
                                    withMul(a_td, kTen, [&](const auto& aNext) {
                                      withMul(n, kTen, [&](const auto& n10) {
                                        rec(rec, i, n10, d, aNext);
                                      });
                                    });
                                  });
                                } else {
                                  rec(rec, i0, n, d, a);
                                }
                              });
                            });
                          });
                        });
                      });
                    });
                  } else
                    rec(rec, i0, n, d, a);
                });
              });
            });
          });
        });
      });
    }
  };
  rec0(rec0, 0, kOne, kOne, kZero);
}

template<class F>
void withBigInt(int n, F&& f) { f(BigInt(n)); }

template<class F>
void withAdd(const BigInt& lhs, const BigInt& rhs, F&& f) { f(lhs + rhs); }

template<class F>
void withSub(const BigInt& lhs, const BigInt& rhs, F&& f) { f(lhs - rhs); }

template<class F>
void withMul(const BigInt& lhs, const BigInt& rhs, F&& f) { f(lhs * rhs); }

template<class F>
void withDiv(const BigInt& lhs, const BigInt& rhs, F&& f) { f(lhs / rhs); }

int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  PiDigits(n);
  return 0;
}
