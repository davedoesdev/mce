cmake_minimum_required(VERSION 2.8.9)
if (NOT DEFINED ENV{INCLUDEOS_PREFIX})
  set(ENV{INCLUDEOS_PREFIX} /usr/local)
endif()
include($ENV{INCLUDEOS_PREFIX}/includeos/pre.service.cmake)
project (service)

# Human-readable name of your service
set(SERVICE_NAME "State-saving, continuation-passing metacircular evaluator")

# Name of your service binary
set(BINARY "mce")

# Source files to be linked with OS library parts to form bootable image
set(SOURCES
  service.cpp
  nargv.cpp
  ../mce.cpp
  )

# To add your own include paths:
set(LOCAL_INCLUDES "..")

# DRIVERS / PLUGINS:
set(DRIVERS
  # virtionet   # Virtio networking driver
  # virtioblk   # Virtio block device driver
  #boot_logger   # Enable lots of logging from boot stage

  # Use "boot --drivers ." to see other drivers
  )

set(PLUGINS
  # syslogd    # Syslog over UDP

  # Use "boot --plugins ." to see other plugins
  )

# STATIC LIBRARIES:
set(LIBRARIES
  # path to full library
  )

# include service build script
include($ENV{INCLUDEOS_PREFIX}/includeos/post.service.cmake)

get_filename_component(DISK_PATH disk REALPATH BASE_DIR "${CMAKE_SOURCE_DIR}")
set(STATE_PATH "${DISK_PATH}/state")
get_filename_component(TL_PATH ../.. REALPATH BASE_DIR "${CMAKE_SOURCE_DIR}")
set(SCM_PATH "${TL_PATH}/scm")
set(EXPAND_PATH "${SCM_PATH}/expand")
set(SCAN_PATH "${SCM_PATH}/scan")
set(TEST_PATH "${TL_PATH}/examples")
set(TEST_PROG "${TEST_PATH}/test-loop.scm")

file(MAKE_DIRECTORY "${DISK_PATH}")

add_custom_command(
  OUTPUT "${STATE_PATH}"
  COMMAND "${EXPAND_PATH}" < "${TEST_PROG}" | "${SCAN_PATH}" > "${STATE_PATH}"
  DEPENDS "${TEST_PROG}"
  )

add_custom_target(mce_state DEPENDS "${STATE_PATH}")

add_dependencies(service mce_state)

# Create in-memory filesystem from folder
diskbuilder(disk)
