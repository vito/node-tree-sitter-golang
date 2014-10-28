#include "tree_sitter/runtime.h"
#include <node.h>
#include "nan.h"

using namespace v8;

extern "C" TSLanguage * ts_language_go();

namespace tree_sitter_go {

NAN_METHOD(New) {
  NanScope();
  NanReturnUndefined();
}

void InitAll(Handle<Object> exports, Handle<Object> module) {
  Local<FunctionTemplate> tpl = NanNew<FunctionTemplate>(New);
  tpl->SetClassName(NanNew("Language"));
  tpl->InstanceTemplate()->SetInternalFieldCount(1);

  Local<Function> constructor = tpl->GetFunction();
  Local<Object> instance = constructor->NewInstance(0, NULL);
  NanSetInternalFieldPointer(instance, 0, ts_language_go());

  module->Set(NanNew("exports"), instance);
}

NODE_MODULE(ts_language_go_binding, InitAll)

}  // namespace tree_sitter_go
