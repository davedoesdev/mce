#include <memory>
#include <functional>
#include <string>
#include <boost/any.hpp>

typedef boost::any box;
typedef std::shared_ptr<box> boxed;
typedef boxed function(boxed);
typedef std::function<function> func;
typedef std::shared_ptr<func> lambda;

extern size_t gc_threshold;
extern const boxed bnil;

template<typename T>
struct cast_return{ typedef T type; };

template<typename T>
inline typename cast_return<T>::type box_cast(const boxed& a) {
    return boost::any_cast<typename cast_return<T>::type>(*a);
}

template<typename T>
inline bool box_contains(const boxed& a) {
    return a->type() == typeid(T);
}

boxed cons(boxed car, boxed cdr);

boxed mce_restore(const std::string& s);

boxed run(boxed state);
