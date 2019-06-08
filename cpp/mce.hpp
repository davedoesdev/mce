#include <memory>
#include <istream>
#include <string>

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

        placeholder* clone() override {
            return new holder(held);
        }

        bool contains(const std::type_info& ti) override {
            return typeid(T) == ti;
        }

        bool contains_type_of(const Box& b) override {
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

function *get_global_function(const std::string& name);
void register_global_function(const std::string& name, function f);
void register_kenv_function(function f);

boxed get_config(const std::string& k);
void set_config(const std::string& k, boxed v);

extern size_t gc_threshold;

boxed start(std::istream &stream);
boxed start(const std::string &s);

boxed start(int argc, char *argv[]);
