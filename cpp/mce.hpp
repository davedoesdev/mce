#include <memory>
#include <istream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <functional>

template<typename T>
struct box_type { typedef T type; };

class Runtime;

// Adapted from boost::any but without the need for type_traits
class Box {
public:
    explicit Box(std::shared_ptr<Runtime> runtime) :
        content(nullptr),
        runtime(runtime) {
    }

    template<typename T>
    explicit Box(const T& v, std::shared_ptr<Runtime> runtime) :
        content(new holder<T>(v)),
        runtime(runtime) {
    }

    explicit Box(const Box& b) :
        content(b.content ? b.content->clone() : nullptr),
        runtime(b.runtime) {
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

    std::shared_ptr<Runtime> get_runtime() {
        return runtime;
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
    std::shared_ptr<Runtime> runtime;
};

typedef std::shared_ptr<Box> boxed;
typedef std::pair<boxed, boxed> pair;
typedef std::vector<boxed> vector;
typedef boxed function(boxed);
typedef std::function<function> func;
typedef std::shared_ptr<func> lambda;

template<typename R>
R make_lambda(func fn, std::shared_ptr<Runtime> runtime);

class symbol : public std::string {
public:
    symbol(const std::string& s);
};

class Runtime {
public:
    Runtime();

    void set_gc_threshold(size_t v);
    boxed maybe_gc(boxed state);

    void set_config(const std::string& k, boxed v);

    function* get_global_function(const std::string& name);
    void register_global_function(const std::string& name, function f);

    void register_kenv_function(function f);

private:
    std::unordered_map<pair*, std::weak_ptr<pair>> allocated_pairs;
    std::unordered_map<vector*, std::weak_ptr<vector>> allocated_vectors;
    std::unordered_map<func*, std::weak_ptr<func>> allocated_functions;

    size_t gc_threshold;

    std::unordered_map<std::string, function*> global_table;
    std::unordered_set<function*> kenvfn_set;
    std::unordered_map<std::string, boxed> config_table;

    friend boxed cons(boxed car, boxed cdr);
    friend boxed make_vector(std::shared_ptr<Runtime> runtime);
    friend lambda make_lambda<lambda>(func fn, std::shared_ptr<Runtime> runtime);
    friend boxed find_global(const symbol& sym, std::shared_ptr<Runtime> runtime);
    friend boxed wrap_global_lambda(boxed fn, boxed cf);
    friend boxed get_config(boxed args);
};

boxed start(std::shared_ptr<Runtime> runtime, std::istream &stream);
boxed start(std::shared_ptr<Runtime> runtime, const std::string &s);
boxed start(int argc, char *argv[]);
