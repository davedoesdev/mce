#include <memory>
#include <istream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <functional>

namespace mce {

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
        if (empty()) {
            throw std::range_error("bad box cast");
        }
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
typedef std::shared_ptr<std::vector<boxed>> vector;
typedef boxed function(boxed);
typedef std::shared_ptr<std::function<function>> func;
typedef std::shared_ptr<func> lambda;

template<typename R>
R make_lambda(std::function<function> fn,
              std::shared_ptr<Runtime> runtime,
              bool has_defn = false);

class symbol : public std::string {
public:
    symbol(const std::string& s);
};

class Runtime : public std::enable_shared_from_this<Runtime> {
private:
    size_t gc_threshold;
    void break_cycles();
    void add_stats(std::vector<std::vector<size_t>>& stats);
    boxed gc_callback;
    bool calling_gc_callback;

    struct {
        std::unordered_map<vector*, std::weak_ptr<vector>> vectors;
        std::unordered_map<func*, std::pair<bool, std::weak_ptr<func>>> functions;
    } allocated;

    std::unordered_map<std::string, function*> global_table;
    std::vector<function*> core_globals;
    std::unordered_set<function*> kenvfn_set;
    std::unordered_map<std::string, boxed> config_table;

    friend boxed make_vector(std::shared_ptr<Runtime> runtime);
    friend lambda make_lambda<lambda>(std::function<function> fn,
                                      std::shared_ptr<Runtime> runtime,
                                      bool has_defn);
    friend boxed find_global(const symbol& sym, std::shared_ptr<Runtime> runtime);
    friend boxed find_global(const double i, std::shared_ptr<Runtime> runtime);
    friend boxed wrap_global_lambda(boxed fn, boxed cf);

public:
    Runtime();

    void set_gc_threshold(size_t v);
    void maybe_gc();

    boxed get_config(const std::string& k);
    void set_config(const std::string& k, boxed v);

    void set_gc_callback(boxed v);

    function* get_global_function(const std::string& name);
    void register_global_function(const std::string& name, function f);
    void unregister_global_function(const std::string& name);

    void register_kenv_function(function f);

    const double g_result;
};

boxed send(boxed k, boxed args);
boxed sendv(boxed k, boxed v);
boxed start(std::istream &stream, boxed args);
boxed start(const std::string &s, boxed args);
boxed start(int argc, char *argv[]);

}
