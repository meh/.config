#! /system/bin/ash
IMAGE="/data/local/arch.img"
MOUNT="/data/local/arch"
SHELL="/bin/zsh"
SDCARD="$(readlink -f /sdcard)"

if [[ "$1" == "close" ]]; then
  rm -f "$MOUNT/dev/fd"

  for i in /proc/*; do
    [ -d "$i" ] && readlink -f "$i/root" | grep -qF "$MOUNT" && kill -9 "$(basename "$i")"
  done

  if mountpoint -q "$MOUNT/sdcard"; then
    umount "$MOUNT/sdcard"
  fi

  if mountpoint -q "$MOUNT/sys"; then
    umount "$MOUNT/sys" || exit 1
  fi

  if mountpoint -q "$MOUNT/proc"; then
    umount "$MOUNT/proc" || exit 1
  fi

  if mountpoint -q "$MOUNT/dev/pts"; then
    umount "$MOUNT/dev/pts" || exit 1
  fi

  if mountpoint -q "$MOUNT/dev"; then
    umount "$MOUNT/dev" || exit 1
  fi

  rm -rf "$MOUNT/tmp/*"

  if mountpoint -q "$MOUNT"; then
    umount "$MOUNT" || exit 1
  fi

  exit
fi

if [[ ! -d "$MOUNT" ]]; then
  rm -f "$MOUNT"
  mkdir "$MOUNT"
fi

if ! mountpoint -q "$MOUNT"; then
  if which e2fsck > /dev/null; then
    e2fsck -y "$IMAGE"

    case $? in
      [012]) ;;

      *) echo "Image is damaged, can't mount it" >&2
        exit 1
        ;;
    esac
  fi

  mount "$IMAGE" "$MOUNT" || exit 1
  rm -rf "$MOUNT/tmp/*"
fi

mountpoint -q "$MOUNT/proc"    || mount -t proc proc "$MOUNT/proc"
mountpoint -q "$MOUNT/sys"     || mount -t sysfs sysfs "$MOUNT/sys"
mountpoint -q "$MOUNT/dev"     || mount -o bind /dev "$MOUNT/dev"
mountpoint -q "$MOUNT/dev/pts" || mount -o bind /dev/pts "$MOUNT/dev/pts"

if mountpoint -q "$SDCARD"; then
  [ -d "$MOUNT/sdcard" ] || mkdir -p "$MOUNT/sdcard"
  mountpoint -q "$MOUNT/sdcard" || mount -o bind "$SDCARD" "$MOUNT/sdcard"
fi

[[ -e "$MOUNT/dev/fd" ]] || ln -s /proc/self/fd "$MOUNT/dev/fd"

chroot "$MOUNT" "$SHELL"
