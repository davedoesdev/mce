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
extern const boxed nil;

template<typename T>
inline T box_cast(const boxed& a) {
    return boost::any_cast<T>(*a);
}

inline bool box_contains_lambda(const boxed& a) {
    return a->type() == typeid(lambda);
}

boxed cons(boxed car, boxed cdr);

boxed mce_restore(const std::string& s);

boxed run(boxed state);
