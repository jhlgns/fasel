#pragma once

#include <iostream>
#include <utility>

using namespace std::string_view_literals;

template<typename F>
struct ScopeExit
{
    F f;
    explicit ScopeExit(F f)
        : f(f)
    {
    }
    ~ScopeExit() { f(); }
};

struct DEFER_TAG
{
};

template<class F>
ScopeExit<F> operator+(DEFER_TAG /*unused*/, F &&f)
{
    return ScopeExit<F>{std::forward<F>(f)};
}

#define CONCAT_1(x, y) x##y
#define CONCAT_2(x, y) CONCAT_1(x, y)

#define defer auto CONCAT_2(ScopeExit, __LINE__) = DEFER_TAG{} + [&]()

template<typename T>
struct TempSwap
{
    T &destination;
    T old_value;

    template<typename U>
    explicit TempSwap(T &destination, U &&new_value)
        : destination{destination}
        , old_value{std::move(destination)}
    {
        destination = std::move(new_value);
    }

    ~TempSwap() { this->destination = std::move(this->old_value); }
};

#define SET_TEMPORARILY(variable, new_value) TempSwap CONCAT_2(TempSwap, __LINE__){variable, new_value};

#define ENSURE(condition)                         \
    if (!(condition))                             \
    {                                             \
        std::cout << "Check failed" << std::endl; \
        std::abort();                             \
    }

#define UNREACHED                                            \
    do                                                       \
    {                                                        \
        std::cout << "UNREACHED macro reached" << std::endl; \
        std::abort();                                        \
    } while (false);

#define TODO                                            \
    do                                                  \
    {                                                   \
        std::cout << "TODO macro reached" << std::endl; \
        std::abort();                                   \
    } while (false);

#define FATAL(message)                     \
    do                                     \
    {                                      \
        std::cout << message << std::endl; \
        std::abort();                      \
    } while (false);
