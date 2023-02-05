#include <cassert>
#include <concepts>
#include <exception>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

template <typename... Types>
struct variant;

template <typename Visitor, typename... Variants>
constexpr auto visit(Visitor&& vis, Variants&&... vars);

namespace storage {
template <bool is_trivial_destr, typename... Types>
struct variant_storage;
}

inline constexpr std::size_t variant_npos = -1;

struct monostate {};
constexpr bool operator==(const monostate&, const monostate&) noexcept {
  return true;
}
constexpr bool operator!=(const monostate&, const monostate&) noexcept {
  return false;
}
constexpr bool operator<(const monostate&, const monostate&) noexcept {
  return false;
}
constexpr bool operator>(const monostate&, const monostate&) noexcept {
  return false;
}
constexpr bool operator<=(const monostate&, const monostate&) noexcept {
  return true;
}
constexpr bool operator>=(const monostate&, const monostate&) noexcept {
  return true;
}

struct bad_variant_access : public std::exception {
public:
  bad_variant_access() noexcept = default;
  bad_variant_access(const bad_variant_access&) noexcept = default;
  bad_variant_access& operator=(const bad_variant_access&) noexcept = default;

  const char* what() const noexcept override {
    return "bad_variant_access";
  }
};

template <typename T>
struct variant_size;

template <bool T, typename... Types>
struct variant_size<storage::variant_storage<T, Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<const T> : variant_size<T> {};

template <typename T>
struct variant_size<volatile T> : variant_size<T> {};

template <typename T>
struct variant_size<const volatile T> : variant_size<T> {};

template <typename T>
inline constexpr std::size_t variant_size_v = variant_size<T>::value;

template <std::size_t I, typename T>
struct variant_alternative;

template <std::size_t I, typename T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

template <std::size_t I, bool T, typename... Types>
struct variant_alternative<I, storage::variant_storage<T, Types...>> : variant_alternative<I, variant<Types...>> {};

template <std::size_t I, typename Head, typename... Tail>
struct variant_alternative<I, variant<Head, Tail...>> : variant_alternative<I - 1, variant<Tail...>> {};

template <typename Head, typename... Tail>
struct variant_alternative<0, variant<Head, Tail...>> {
  using type = Head;
};

template <std::size_t I, typename T>
struct variant_alternative<I, volatile T> {
  using type = volatile variant_alternative_t<I, T>;
};

template <std::size_t I, typename T>
struct variant_alternative<I, const T> {
  using type = const variant_alternative_t<I, T>;
};

template <std::size_t I, typename T>
struct variant_alternative<I, volatile const T> {
  using type = volatile const variant_alternative_t<I, T>;
};

template <class T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <class T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

namespace finder {

template <std::size_t Ind, typename... Types>
struct find_raw_alternative {
  using type = void;
};

template <std::size_t Ind, typename... Types>
requires(Ind < sizeof...(Types)) struct find_raw_alternative<Ind, Types...> {
  using type = variant_alternative_t<Ind, variant<Types...>>;
};

template <typename Find, typename... Types>
struct find_index;

template <typename Find>
struct find_index<Find> {
  static constexpr std::size_t value = -1;
};

template <typename Find, typename Head, typename... Tail>
struct find_index<Find, Head, Tail...> {
  static constexpr std::size_t value = (std::is_same_v<Find, Head> ? 0 : find_index<Find, Tail...>::value + 1);
};

template <typename Find, typename... Types>
inline constexpr std::size_t find_index_v = find_index<Find, Types...>::value;

template <typename Find, typename... Types>
struct number_of_entries;

template <typename Find>
struct number_of_entries<Find> {
  static constexpr std::size_t amount = 0;
};

template <typename Find, typename Head, typename... Tail>
struct number_of_entries<Find, Head, Tail...> {
  static constexpr std::size_t amount = number_of_entries<Find, Tail...>::amount + (std::is_same_v<Find, Head> ? 1 : 0);
};

template <typename Find, typename... Types>
inline constexpr bool is_one_occurrence = number_of_entries<Find, Types...>::amount == 1;

} // namespace finder

namespace alternative_chooser {
template <typename T>
struct imagine_array {
  T x[1];
};

template <typename T_i, typename T_arg>
concept imagine = requires {
  imagine_array<T_i>{{std::declval<T_arg>()}};
};

template <typename T_i, typename T_arg, std::size_t I>
struct imaginary_func {
  static std::integral_constant<std::size_t, I> imag(T_i) requires imagine<T_i, T_arg>;
};

template <typename T_arg, typename Variant, typename = std::make_index_sequence<variant_size_v<Variant>>>
struct imaginary_funcs;

template <typename T_arg, typename... T_i, std::size_t... I>
struct imaginary_funcs<T_arg, variant<T_i...>, std::index_sequence<I...>> : imaginary_func<T_i, T_arg, I>... {
  using imaginary_func<T_i, T_arg, I>::imag...;
};

template <typename T_arg, typename Variant>
using choosen_type = decltype(imaginary_funcs<T_arg, Variant>::imag(std::declval<T_arg>()));

template <typename T_arg, typename Variant>
struct choosen_index : std::integral_constant<std::size_t, variant_npos> {};

template <typename T_arg, typename... Types>
concept can_choose_overload = requires {
  typename choosen_type<T_arg, variant<Types...>>;
};

template <typename T_arg, typename... Types>
requires can_choose_overload<T_arg, Types...> struct choosen_index<T_arg, variant<Types...>>
    : choosen_type<T_arg, variant<Types...>> {};

template <typename T_arg, typename Variant>
inline constexpr std::size_t choosen_index_v = choosen_index<T_arg, Variant>::value;

static_assert(choosen_index_v<int, variant<float, double, long>> == 2);
static_assert(choosen_index_v<char*, variant<std::string>> == 0);
static_assert(choosen_index_v<char*, variant<std::string, std::string>> == -1);
static_assert(choosen_index_v<char*, variant<std::string, char*>> == 1);
static_assert(choosen_index_v<const char*, variant<std::string, bool>> == 0);
static_assert(choosen_index_v<double, variant<int, double>> == 1);
} // namespace alternative_chooser

namespace special_members_concepts {

template <typename... Types>
concept copy_cstr = (std::copy_constructible<Types> && ...);

template <typename... Types>
concept move_cstr = (std::move_constructible<Types> && ...);

template <typename... Types>
concept triv_copy_cstr = copy_cstr<Types...> && (std::is_trivially_copy_constructible_v<Types> && ...);

template <typename... Types>
concept triv_move_cstr = move_cstr<Types...> && (std::is_trivially_move_constructible_v<Types> && ...);

template <typename... Types>
concept copy_asg = copy_cstr<Types...> && (std::is_copy_assignable_v<Types> && ...);

template <typename... Types>
concept move_asg = move_cstr<Types...> && (std::is_move_assignable_v<Types> && ...);

template <typename... Types>
concept triv_copy_asg =
    triv_copy_cstr<Types...> && copy_asg<Types...> && (std::is_trivially_copy_assignable_v<Types> && ...);

template <typename... Types>
concept triv_move_asg =
    triv_move_cstr<Types...> && move_asg<Types...> && (std::is_trivially_move_assignable_v<Types> && ...);

} // namespace special_members_concepts

namespace storage {

struct unit_storage_tag {};

template <bool is_triv_destr, typename... Types>
union base_storage {
  constexpr base_storage() = default;
  constexpr base_storage(unit_storage_tag) {}
};

template <typename Head, typename... Tail>
union base_storage<true, Head, Tail...> {

  constexpr base_storage() : val() {}
  constexpr base_storage(unit_storage_tag tag) : rest(tag) {}

  template <std::size_t I, typename... Args>
  constexpr base_storage(in_place_index_t<I>, Args&&... args)
      : rest(in_place_index<I - 1>, std::forward<Args>(args)...) {}

  template <typename... Args>
  constexpr base_storage(in_place_index_t<0>, Args&&... args) : val(std::forward<Args>(args)...) {}

  ~base_storage() = default;

  template <std::size_t I, typename... Args>
  constexpr auto& construct_type(in_place_index_t<I>, Args&&... args) {
    return rest.construct_type(in_place_index<I - 1>, std::forward<Args>(args)...);
  }

  template <typename... Args>
  constexpr auto& construct_type(in_place_index_t<0>, Args&&... args) {
    std::construct_at((const_cast<std::remove_cv_t<Head>*>(std::addressof(val))), std::forward<Args>(args)...);
    return val;
  }

  template <size_t I>
  constexpr auto& get(in_place_index_t<I>) noexcept {
    return rest.get(in_place_index<I - 1>);
  }

  constexpr auto& get(in_place_index_t<0>) noexcept {
    return val;
  }

  template <size_t I>
  constexpr const auto& get(in_place_index_t<I>) const noexcept {
    return rest.get(in_place_index<I - 1>);
  }

  constexpr const auto& get(in_place_index_t<0>) const noexcept {
    return val;
  }

private:
  Head val;
  base_storage<true, Tail...> rest;
};

// Todo try to unite using prospective destructor for all types when clang became 15
template <typename Head, typename... Tail>
union base_storage<false, Head, Tail...> {
  constexpr base_storage() : val(){};
  constexpr base_storage(unit_storage_tag tag) : rest(tag) {}

  template <std::size_t I, typename... Args>
  constexpr base_storage(in_place_index_t<I>, Args&&... args)
      : rest(in_place_index<I - 1>, std::forward<Args>(args)...) {}

  template <typename... Args>
  constexpr base_storage(in_place_index_t<0>, Args&&... args) : val(std::forward<Args>(args)...) {}

  ~base_storage() {}

  template <std::size_t I, typename... Args>
  constexpr auto& construct_type(in_place_index_t<I>, Args&&... args) {
    return rest.construct_type(in_place_index<I - 1>, std::forward<Args>(args)...);
  }

  template <typename... Args>
  constexpr auto& construct_type(in_place_index_t<0>, Args&&... args) {
    std::construct_at((const_cast<std::remove_cv_t<Head>*>(std::addressof(val))), std::forward<Args>(args)...);
    return val;
  }

  template <size_t I>
  constexpr auto& get(in_place_index_t<I>) noexcept {
    return rest.get(in_place_index<I - 1>);
  }

  constexpr auto& get(in_place_index_t<0>) noexcept {
    return val;
  }

  template <size_t I>
  constexpr const auto& get(in_place_index_t<I>) const noexcept {
    return rest.get(in_place_index<I - 1>);
  }

  constexpr const auto& get(in_place_index_t<0>) const noexcept {
    return val;
  }

private:
  Head val;
  base_storage<false, Tail...> rest;
};

template <typename... Types>
using variant_storage_t = variant_storage<(std::is_trivially_destructible_v<Types> && ...), Types...>;

template <typename... Types>
struct variant_storage<true, Types...> {
  template <std::size_t I, typename... Ts>
  friend constexpr variant_alternative_t<I, variant_storage_t<Ts...>>& get(variant_storage_t<Ts...>& v);

  template <typename T, typename... Ts, std::size_t I>
  friend constexpr T& get(variant_storage_t<Ts...>& v);

public:
  constexpr std::size_t index() const noexcept {
    return ind;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

protected:
  constexpr variant_storage() : storage(), ind(0){};
  constexpr variant_storage(unit_storage_tag tag) : storage(tag), ind(variant_npos) {}

  template <std::size_t I, typename... Args>
  constexpr variant_storage(in_place_index_t<I>, Args&&... args)
      : storage(in_place_index<I>, std::forward<Args>(args)...), ind(I) {}

  template <std::size_t I, typename... Args>
  constexpr auto& construct_type(in_place_index_t<I>, Args&&... args) {
    auto& str = storage.construct_type(in_place_index<I>, std::forward<Args>(args)...);
    ind = I;
    return str;
  }

  constexpr ~variant_storage() = default;

  constexpr void reset() {
    ind = variant_npos;
  };

  // for not making a lot of getters (see how it used in variant)
  base_storage<true, Types...> storage;
  std::size_t ind;
};

template <typename... Types>
struct variant_storage<false, Types...> {
  template <std::size_t I, typename... Ts>
  friend constexpr variant_alternative_t<I, variant_storage_t<Ts...>>& get(variant_storage_t<Ts...>& v);

  template <typename T, typename... Ts, std::size_t I>
  friend constexpr T& get(variant_storage_t<Ts...>& v);

public:
  constexpr std::size_t index() const noexcept {
    return ind;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

protected:
  constexpr variant_storage() : storage(), ind(0){};
  constexpr variant_storage(unit_storage_tag tag) : storage(tag), ind(variant_npos) {}

  template <std::size_t I, typename... Args>
  constexpr variant_storage(in_place_index_t<I>, Args&&... args)
      : storage(in_place_index<I>, std::forward<Args>(args)...), ind(I) {}

  template <std::size_t I, typename... Args>
  constexpr auto& construct_type(in_place_index_t<I>, Args&&... args) {
    auto& str = storage.construct_type(in_place_index<I>, std::forward<Args>(args)...);
    ind = I;
    return str;
  }

  constexpr void reset() {
    if (ind != variant_npos) {
      visit([]<typename T>(T& t) { t.~T(); }, *this);
      ind = variant_npos;
    }
  }

  constexpr ~variant_storage() {
    reset();
  };

  // for not making a lot of getters (see how it used in variant)
  base_storage<false, Types...> storage;
  std::size_t ind;
};

template <typename T, typename... Types>
constexpr bool holds_alternative(const variant_storage_t<Types...>& v) noexcept
    requires(finder::is_one_occurrence<T, Types...>) {
  return v.index() == finder::find_index_v<T, Types...>;
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant_storage_t<Types...>>& get(variant_storage_t<Types...>& v) {
  if (I == v.index()) {
    return v.storage.get(in_place_index<I>);
  } else {
    throw bad_variant_access();
  }
}

template <std::size_t I, typename... Types>
constexpr variant_alternative_t<I, variant_storage_t<Types...>>&& get(variant_storage_t<Types...>&& v) {
  return std::move(get<I>(v));
}

template <std::size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant_storage_t<Types...>>& get(const variant_storage_t<Types...>& v) {
  return get<I>(const_cast<variant_storage_t<Types...>&>(v));
}

template <std::size_t I, typename... Types>
constexpr const variant_alternative_t<I, variant_storage_t<Types...>>&& get(const variant_storage_t<Types...>&& v) {
  return std::move(get<I>(v));
}

template <typename T, typename... Types, std::size_t I = finder::find_index_v<T, Types...>>
constexpr T& get(variant_storage_t<Types...>& v) {
  if (holds_alternative<T>(v)) {
    return v.storage.get(in_place_index<I>);
  } else {
    throw bad_variant_access();
  }
}

template <typename T, typename... Types>
constexpr T&& get(variant_storage_t<Types...>&& v) {
  return std::move(get<T>(v));
};

template <typename T, typename... Types, std::size_t I = finder::find_index_v<T, Types...>>
constexpr const T& get(const variant_storage_t<Types...>& v) {
  return get<T>(const_cast<variant_storage_t<Types...>&>(v));
}

template <typename T, typename... Types>
constexpr const T&& get(const variant_storage_t<Types...>&& v) {
  return std::move(get<T>(v));
}
} // namespace storage

namespace visitor {
// multidimensional table
template <typename T, std::size_t... Idx>
struct multiarray {
  T t;

  constexpr T at() const {
    return t;
  }
};

template <typename T, std::size_t Head, std::size_t... Tail>
struct multiarray<T, Head, Tail...> {
  multiarray<T, Tail...> arr[Head];

  constexpr T at(std::size_t i0, auto... idx) const {
    return arr[i0].at(idx...);
  }
};

template <std::size_t... Idx>
struct first_index_to_seq {
  using type = std::index_sequence<>;
};

template <std::size_t First, std::size_t... Idx>
struct first_index_to_seq<First, Idx...> {
  using type = std::make_index_sequence<First>;
};

template <std::size_t... Idx>
using first_index_to_seq_t = typename first_index_to_seq<Idx...>::type;

// table for visiting variants depending on idx in variants
//  Address - cur dimension:  a.at(Addres...)
//  Remain - sizes of remain variant alternatives
//  Helper - just index_sequnce type for using expansion in generator
//  Visitor - visitor
// generate() - generate multidimensional caller table
template <typename Address, typename Remain, typename Helper, typename Func>
struct visitor_table;

template <std::size_t... Address, std::size_t First, std::size_t... Remain, std::size_t... Helper, typename Func>
struct visitor_table<std::index_sequence<Address...>, std::index_sequence<First, Remain...>,
                     std::index_sequence<Helper...>, Func> {
  static constexpr multiarray<Func, First, Remain...> generate() {
    return {visitor_table<std::index_sequence<Address..., Helper>, std::index_sequence<Remain...>,
                          first_index_to_seq_t<Remain...>, Func>::generate()...};
  }
};

template <std::size_t... Address, typename Result, typename Visitor>
struct visitor_table<std::index_sequence<Address...>, std::index_sequence<>, std::index_sequence<>,
                     Result (*)(Visitor)> {
  using Func = Result (*)(Visitor);

  static constexpr Func generate() {
    return [](Visitor vis) {
      // just because invoke_result_t need types
      return std::forward<Visitor>(vis)(std::integral_constant<std::size_t, Address>()...);
    };
  };
};

template <typename>
inline constexpr std::size_t zero_expanded = 0;

template <typename Visitor, typename... Variants>
inline constexpr auto visitor_table_generated =
    visitor_table<std::index_sequence<>, std::index_sequence<variant_size_v<std::remove_reference_t<Variants>>...>,
                  first_index_to_seq_t<variant_size_v<std::remove_reference_t<Variants>>...>,
                  std::invoke_result_t<Visitor, std::integral_constant<std::size_t, zero_expanded<Variants>>...> (*)(
                      Visitor)>::generate();

template <typename Visitor, typename... Variants>
constexpr auto visit_indexed(Visitor&& vis, Variants&&... vars) {
  return visitor_table_generated<Visitor, Variants...>.at(vars.index()...)(std::forward<Visitor>(vis));
}
} // namespace visitor

template <typename Visitor, typename... Variants>
constexpr auto visit(Visitor&& vis, Variants&&... vars) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  return visitor::visit_indexed(
      [&](auto... idx) -> std::invoke_result_t<Visitor, decltype(get<0>(std::declval<Variants>()))...> {
        return std::forward<Visitor>(vis)(get<idx>(std::forward<Variants>(vars))...);
      },
      std::forward<Variants>(vars)...);
}

template <typename R, typename Visitor, typename... Variants>
constexpr R visit(Visitor&& vis, Variants&&... vars) {
  return visit(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}
