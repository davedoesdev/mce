// This file is a part of the IncludeOS unikernel - www.includeos.org
//
// Copyright 2015 Oslo and Akershus University College of Applied Sciences
// and Alfred Bratterud
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <service>
#include <iostream>
#include <fs/disk.hpp>
#include <fs/vfs.hpp>
#include <memdisk>
#include <fstream>
#include "nargv.h"
#include "mce.hpp"

fs::Disk_ptr& memdisk() {
  static auto disk = fs::shared_memdisk();
  if (not disk->fs_ready()) {
    disk->init_fs([](fs::error_t err, fs::File_system&) {
      if (err)
        panic("error mounting disk");
    });
  }
  return disk;
}

void Service::start(const std::string& cmdline) {
    auto nvp = nargv_parse(cmdline.c_str());
    if (nvp->error_code) {
        std::cerr << nvp->error_message << std::endl;
        return;
    }

    auto cont = config(nvp->argc, nvp->argv);
    nargv_free(nvp);
    if (!cont) {
        return;
    }

    auto disk = memdisk()->fs().stat("/");
    fs::mount("/mce", disk, "mce");
    std::ifstream is("/mce/state");

    ::start(is);
}
