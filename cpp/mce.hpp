#include <memory>
#include <functional>
#include <string>
#include <istream>

template<typename T>
struct box_type { typedef T type; };

// Adapted from boost::any but without the need for type_traits
class Box {
public:
    Box() : content(nullptr) {
    }

    template<typename T>
    explicit Box(const T& v) : content(new holder<T>(v)) {
    }

    explicit Box(const Box& b) :
        content(b.content ? b.content->clone() : nullptr) {
    }

    ~Box() {
        delete content;
    }

    Box& operator=(const Box& b) {
        Box(b).swap(*this);
        return *this;
    }

    bool empty() {
        return content == nullptr;
    }

    template<typename T>
    bool contains() const {
        return content &&
               content->contains(typeid(typename box_type<T>::type));
    }

    bool contains_type_of(const Box& b) {
        return content && content->contains_type_of(b);
    }

    template<typename T>
    typename box_type<T>::type cast() {
        return *static_cast<typename box_type<T>::type*>(
            content->address(typeid(typename box_type<T>::type)));
    }

private:
    void swap(Box& b) {
        std::swap(content, b.content);
    }

    class placeholder {
    public:
        virtual ~placeholder() {}
        virtual placeholder* clone() = 0;
        virtual bool contains(const std::type_info&) = 0;
        virtual bool contains_type_of(const Box& b) = 0;
        virtual void* address(const std::type_info&) = 0;
    };

    template<typename T>
    class holder : public placeholder {
    public:
        holder(const T& v) : held(v) {}

        placeholder* clone() {
            return new holder(held);
        }

        bool contains(const std::type_info& ti) override {
            return typeid(T) == ti;
        }

        bool contains_type_of(const Box& b) {
            return b.contains<T>();
        }

        void* address(const std::type_info& ti) override {
            if (typeid(T) != ti) {
                throw std::range_error("bad box cast");
            }

            return &held;
        }

    private:
        T held;
    };

    placeholder *content;
};

typedef std::shared_ptr<Box> boxed;
typedef boxed function(boxed);
typedef std::function<function> func;
typedef std::shared_ptr<func> lambda;

extern size_t gc_threshold;
extern const boxed bnil;

boxed cons(boxed car, boxed cdr);

boxed mce_restore(const std::string& s);

boxed run(boxed state);

bool config(int argc, char *argv[]);

boxed start(std::istream &stream);
