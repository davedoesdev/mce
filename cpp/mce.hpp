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
class Box;
typedef std::shared_ptr<Box> boxed;

class RuntimeInfo {
public:
    explicit RuntimeInfo();

    inline std::shared_ptr<Runtime> get_runtime() const {
        return runtime;
    }

    inline boxed get_nil() const {
        return nil;
    }

private:
    explicit RuntimeInfo(std::shared_ptr<Runtime> runtime, boxed nil);

    std::shared_ptr<Runtime> runtime;
    boxed nil;

    friend class Box;
};

// Adapted from boost::any but without the need for type_traits
class Box : public std::enable_shared_from_this<Box> {
public:
    template<typename T>
    explicit Box(const T& v, const RuntimeInfo& info) :
        content(new holder<T>(v)),
        info(info) {
    }

    explicit Box(const Box& b) :
        std::enable_shared_from_this<Box>(),
        content(b.content->clone()),
        info(b.info) {
    }

    ~Box() {
        delete content;
    }

    Box& operator=(const Box& b) {
        Box(b).swap(*this);
        return *this;
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
        if (content == nullptr) {
            throw std::range_error("bad box cast");
        }
        return *static_cast<typename box_type<T>::type*>(
            content->address(typeid(typename box_type<T>::type)));
    }

    inline RuntimeInfo get_runtime_info() {
        if (info.get_nil()) {
            return info;
        }
        return RuntimeInfo(info.get_runtime(), shared_from_this());
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
    RuntimeInfo info;
};

typedef std::shared_ptr<std::vector<boxed>> vector;
typedef boxed function(boxed);
typedef std::shared_ptr<std::function<function>> func;
typedef std::shared_ptr<func> lambda;

template<typename R>
R make_lambda(std::function<function> fn,
              const RuntimeInfo& info,
              bool has_defn = false);

class Runtime {
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

    std::unordered_map<std::string, boxed> config_table;

    friend boxed make_vector(const RuntimeInfo& info);
    friend lambda make_lambda<lambda>(std::function<function> fn,
                                      const RuntimeInfo& info,
                                      bool has_defn);

    std::unordered_map<std::string, function*> global_table;
    std::vector<function*> core_globals;
    std::unordered_set<function*> kenvfn_set;

public:
    explicit Runtime();

    void set_gc_threshold(size_t v);
    void maybe_gc(boxed x);

    boxed get_config(const std::string& k);
    void set_config(const std::string& k, boxed v);

    void set_gc_callback(boxed v);

    function* get_global_function(const std::string& name);
    function* get_global_function(const double i);
    void register_global_function(const std::string& name, function f);
    void unregister_global_function(const std::string& name);

    void register_kenv_function(function f);
    bool is_kenv_function(function f);

    const double g_result;
};

boxed send(boxed k, boxed args);
boxed sendv(boxed k, boxed v);
boxed start(std::istream &stream, boxed args);
boxed start(const std::string &s, boxed args);
boxed start(int argc, char *argv[]);

}
