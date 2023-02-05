#pragma once
#include "helper-classes.h"

template <typename... Types>
struct variant : storage::variant_storage_t<Types...> {
private:
  using base = storage::variant_storage_t<Types...>;

public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<variant_alternative_t<0, variant<Types...>>>)
      requires(std::is_default_constructible_v<variant_alternative_t<0, variant<Types...>>>) = default;

  constexpr ~variant() = default;

  constexpr variant(const variant& rhs) requires(special_members_concepts::copy_cstr<Types...>)
      : base(storage::unit_storage_tag()) {
    if (!rhs.valueless_by_exception()) {
      visitor::visit_indexed([this, &rhs](auto index) { this->construct_type(in_place_index<index>, get<index>(rhs)); },
                             rhs);
    }
  }

  constexpr variant(const variant& rhs) requires(special_members_concepts::triv_copy_cstr<Types...>) = default;

  constexpr variant(variant&& rhs) noexcept((std::is_nothrow_move_constructible_v<Types> && ...))
      requires(special_members_concepts::move_cstr<Types...>)
      : base(storage::unit_storage_tag()) {
    if (!rhs.valueless_by_exception()) {
      visitor::visit_indexed(
          [this, &rhs](auto index) { this->construct_type(in_place_index<index>, std::move(get<index>(rhs))); }, rhs);
    }
  }

  constexpr variant(variant&& rhs) noexcept((std::is_nothrow_move_constructible_v<Types> && ...))
      requires(special_members_concepts::triv_move_cstr<Types...>) = default;

  constexpr variant& operator=(variant&& rhs) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                                         std::is_nothrow_move_assignable_v<Types>)&&...))
      requires(special_members_concepts::move_asg<Types...>) {
    if (rhs.valueless_by_exception()) {
      base::reset();
      return *this;
    }
    visitor::visit_indexed(
        [this, &rhs](auto index_) {
          if (base::index() == index_) {
            get<index_>(*this) = get<index_>(std::move(rhs));
          } else {
            emplace<index_>(get<index_>(std::move(rhs)));
          }
        },
        rhs);
    return *this;
  }

  constexpr variant& operator=(variant&& rhs) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                                         std::is_nothrow_move_assignable_v<Types>)&&...))
      requires(special_members_concepts::triv_move_asg<Types...>) = default;

  constexpr variant& operator=(const variant& rhs) requires(special_members_concepts::copy_asg<Types...>) {
    if (rhs.valueless_by_exception()) {
      base::reset();
      return *this;
    }
    visitor::visit_indexed(
        [this, &rhs](auto l_ind, auto r_ind) {
          if constexpr (l_ind == r_ind) {
            get<l_ind>(*this) = get<r_ind>(rhs);
          } else {
            using alt = variant_alternative_t<r_ind, variant<Types...>>;
            if constexpr (std::is_nothrow_copy_constructible_v<alt> || !std::is_nothrow_move_constructible_v<alt>) {
              emplace<r_ind>(get<r_ind>(rhs));
            } else {
              this->operator=(variant(rhs));
            }
          }
        },
        *this, rhs);
    return *this;
  }

  constexpr variant& operator=(const variant& rhs)
      requires(special_members_concepts::triv_copy_asg<Types...>) = default;

  template <typename T, std::size_t index = alternative_chooser::choosen_index_v<T, variant<Types...>>,
            typename T_j = typename finder::find_raw_alternative<index, Types...>::type>
  constexpr variant(T&& t) noexcept(std::is_nothrow_constructible_v<T_j, T>) requires(std::is_constructible_v<T_j, T>)
      : base(in_place_index<index>, std::forward<T>(t)) {}

  template <typename T, std::size_t index_ = alternative_chooser::choosen_index_v<T, variant<Types...>>,
            typename T_j = typename finder::find_raw_alternative<index_, Types...>::type>
  constexpr variant&
  operator=(T&& t) noexcept(std::is_nothrow_assignable_v<T_j&, T>&& std::is_nothrow_constructible_v<T_j, T>)
      requires(std::is_assignable_v<T_j&, T>&& std::is_constructible_v<T_j, T>) {
    if (index_ == base::index()) {
      get<index_>(*this) = std::forward<T>(t);
    } else {
      if constexpr (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>) {
        emplace<index_>(std::forward<T>(t));
      } else {
        emplace<index_>(T_j(std::forward<T>(t)));
      }
    }
    return *this;
  }

  template <typename T, std::size_t index = finder::find_index_v<T, Types...>, typename... Args>
  constexpr explicit variant(in_place_type_t<T>, Args&&... args)
      requires(std::is_constructible_v<T, Args...>&& finder::is_one_occurrence<T, Types...>)
      : base(in_place_index<index>, std::forward<Args>(args)...) {}

  template <std::size_t I, typename... Args>
  constexpr explicit variant(in_place_index_t<I>, Args&&... args)
      requires(I < sizeof...(Types) &&
               std::is_constructible_v<typename finder::find_raw_alternative<I, Types...>::type, Args...>)
      : base(in_place_index<I>, std::forward<Args>(args)...) {}

  template <std::size_t I, typename... Args>
  constexpr variant_alternative_t<I, variant>& emplace(Args&&... args)
      requires(I < sizeof...(Types) && std::is_constructible_v<variant_alternative_t<I, variant<Types...>>, Args...>) {
    base::reset();
    base::construct_type(in_place_index<I>, std::forward<Args>(args)...);
    return get<I>(*this);
  }

  template <typename T, typename... Args, std::size_t I = finder::find_index_v<T, Types...>>
  constexpr T& emplace(Args&&... args)
      requires(finder::is_one_occurrence<T, Types...>&&
                   std::is_constructible_v<variant_alternative_t<I, variant<Types...>>, Args...>) {
    return emplace<I>(std::forward<Args>(args)...);
  }

  constexpr void swap(variant& rhs) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                               std::is_nothrow_swappable_v<Types>)&&...))
      requires(((std::is_move_constructible_v<Types> && std::is_swappable_v<Types>)&&...)) {
    if (base::index() == rhs.index()) {
      if (base::valueless_by_exception() && rhs.valueless_by_exception()) {
        return;
      }
      visitor::visit_indexed(
          [this, &rhs](auto index) {
            using std::swap;
            swap(get<index>(*this), get<index>(rhs));
          },
          rhs);
    } else {
      using std::swap;
      swap(*this, rhs);
    }
  }
};

template <typename T, typename... Types>
constexpr bool holds_alternative(const variant<Types...>& v) noexcept requires(finder::is_one_occurrence<T, Types...>) {
  return storage::holds_alternative<T>(v);
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v) {
  return storage::get<I>(v);
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v) {
  return std::move(get<I>(v));
}

template <std::size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant<Types...>>& get(const variant<Types...>& v) {
  return get<I>(const_cast<variant<Types...>&>(v));
}

template <std::size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant<Types...>>&& get(const variant<Types...>&& v) {
  return std::move(get<I>(v));
}

template <typename T, typename... Types, std::size_t I = finder::find_index_v<T, Types...>>
constexpr T& get(variant<Types...>& v) {
  return storage::get<T>(v);
}

template <typename T, typename... Types>
constexpr T&& get(variant<Types...>&& v) {
  return std::move(get<T>(v));
};

template <typename T, typename... Types, std::size_t I = finder::find_index_v<T, Types...>>
constexpr const T& get(const variant<Types...>& v) {
  return get<T>(const_cast<variant<Types...>&>(v));
}

template <typename T, typename... Types>
constexpr const T&& get(const variant<Types...>&& v) {
  return std::move(get<T>(v));
}

template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
  return (pv && I == pv->index()) ? std::addressof(get<I>(*pv)) : nullptr;
}

template <std::size_t I, typename... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(const variant<Types...>* pv) noexcept {
  return get<I>(const_cast<variant<Types...>*>(pv));
}

template <typename T, typename... Types, std::size_t I = finder::find_index_v<T, Types...>>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
  return get_if<I>(pv);
}

template <typename T, typename... Types, std::size_t I = finder::find_index_v<T, Types...>>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept {
  return get_if<I>(pv);
}

template <typename... Types>
constexpr bool operator==(const variant<Types...>& v, const variant<Types...>& w) {
  if (v.index() != w.index()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  // only one because indexes are same
  return visitor::visit_indexed([&v, &w](auto index) -> bool { return get<index>(v) == get<index>(w); }, v);
}

template <typename... Types>
constexpr bool operator!=(const variant<Types...>& v, const variant<Types...>& w) {
  return !(v == w);
}

template <typename... Types>
constexpr bool operator<(const variant<Types...>& v, const variant<Types...>& w) {
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  return visitor::visit_indexed(
      [&v, &w](auto l_index, auto r_index) -> bool {
        if constexpr (l_index != r_index) {
          return l_index < r_index;
        } else {
          return get<l_index>(v) < get<r_index>(w);
        }
      },
      v, w);
}

template <typename... Types>
constexpr bool operator>(const variant<Types...>& v, const variant<Types...>& w) {
  return w < v;
}

template <typename... Types>
constexpr bool operator<=(const variant<Types...>& v, const variant<Types...>& w) {
  return !(w < v);
}

template <typename... Types>
constexpr bool operator>=(const variant<Types...>& v, const variant<Types...>& w) {
  return !(v < w);
}
