#!/bin/bash
#########################################################
# note :这是本人毕业设计中涉及到的一段Linux脚本               #
#       并以此作为服务端实践选做的作业内容                    #
# Function :Init Virt_Node env shell script             #
# Platform :All Linux Based Platform                    #
# Version  :1.0                                         #
# Date     :2021/01/04                                  #
# Author   :EricWong                                    #
# Contact  :ericwongmoon@outlook.com                    #
#########################################################

# 全局变量设置
global_node_name="Agent"+$(ip addr | grep eth0 | grep inet | awk '{print $2}'| cut -d . -f 4 | cut -d / -f 1)
# Docker Repo Config
global_docker_repo_host="SENSITIVE"
global_docker_repo_username=""
global_docker_repo_password=""
# Redis Config
global_redis_host="SENSITIVE"
global_redis_port="SENSITIVE"
global_redis_password="SENSITIVE"
# DataBase Config
global_db_host="SENSITIVE"
global_db_username="SENSITIVE"
global_db_password="SENSITIVE"
# SSH Connection Config
global_ssh_password="SENSITIVE"

# 2>/dev/null

# 系统初始化
sys_init() {
  # 初始化时区
  timedatectl set-timezone Asia/Shanghai
  # 关闭SELinux
  sed -i "/SELINUX/{s/permissive/disabled/}" /etc/selinux/config
  sed -i "/SELINUX/{s/enforcing/disabled/}" /etc/selinux/config
  # 停用NetworkManager
  systemctl stop NetworkManager
  systemctl disbale NetworkManager
  # 停用防火墙
  systemctl stop firewalld
  systemctl disbale firewalld
  # 更新Hostname
  hostnamectl set-hostname "${global_node_name}"
  sed -i "s/localhost.localdomain/${global_node_name}/" /etc/sysconfig/network
}

install_tools() {
  yum install -y net-tools wget
}

install_docker() {
  # 安装Docker
  yum install -y yum-utils \
    device-mapper-persistent-data \
    lvm2

  yum-config-manager \
    --add-repo \
    http://mirrors.aliyun.com/docker-ce/linux/centos/docker-ce.repo

  yum install -y docker-ce docker-ce-cli containerd.io

  # 启动Docker
  systemctl start docker
  systemctl enable docker

  # 配置Docker
  touch /etc/docker/daemon.json
  cat >/etc/docker/daemon.json <<EOF
{
  "registry-mirrors": [
    "https://registry.docker-cn.com",
    "http://hub-mirror.c.163.com",
    "https://docker.mirrors.ustc.edu.cn"
  ],
  "insecure-registries":["${global_docker_repo_host}"]
}
EOF

  # 配置Docker0网卡
  echo "net.ipv4.ip_forward=1" >>/usr/lib/sysctl.d/00-system.conf
  systemctl restart network

  # 重启Docker服务
  systemctl daemon-reload
  systemctl restart docker

}

install_kvm() {
  # 开启内核支持
  modprobe kvm # 检验开启成功
  # 安装必要软件
  yum install -y virt-* libvirt bridge-utils qemu-img qemu-kvm

  # 配置kvm
  cat >>/etc/sysconfig/libvirtd <<EOF
LIBVIRTD_CONFIG=/etc/libvirt/libvirtd.conf
LIBVIRTD_ARGS="--listen"
EOF

  cat >>/etc/libvirt/libvirtd.conf <<EOF
listen_tls = 0
listen_tcp = 1
tcp_port = "16509"
listen_addr = "0.0.0.0"
auth_tcp = "none"
EOF

  systemctl start libvirtd
  systemctl enable libvirtd

  # 配置KVM网桥
  cp /etc/sysconfig/network-scripts/ifcfg-eth0 /etc/sysconfig/network-scripts/ifcfg-br0
  sed -i '/IPADDR/d' /etc/sysconfig/network-scripts/ifcfg-eth0
  sed -i '/PREFIX/d' /etc/sysconfig/network-scripts/ifcfg-eth0
  sed -i '/GATEWAY/d' /etc/sysconfig/network-scripts/ifcfg-eth0
  sed -i '/DNS1/d' /etc/sysconfig/network-scripts/ifcfg-eth0
  cat >>/etc/sysconfig/network-scripts/ifcfg-eth0 <<EOF
NM_CONTROLLED=no
BRIDGE=br0
EOF

  local newUUID
  newUUID=$(uuidgen br0)
  sed -i '/TYPE/d' /etc/sysconfig/network-scripts/ifcfg-br0
  sed -i '/NAME/d' /etc/sysconfig/network-scripts/ifcfg-br0
  sed -i '/DEVICE/d' /etc/sysconfig/network-scripts/ifcfg-br0
  sed -i '/UUID/d' /etc/sysconfig/network-scripts/ifcfg-br0
  cat >>/etc/sysconfig/network-scripts/ifcfg-br0 <<EOF
TYPE=Bridge
NAME=br0
DEVICE=br0
NM_CONTROLLED=no
UUID=${newUUID}
EOF

  # 重启网络适配网桥br0
  /etc/init.d/network restart

  #KVM StoragePool初始化
  virsh pool-destroy default
  virsh pool-undefine default

  mkdir -p /home/libvirt/vols
  virsh pool-define-as default dir - - - - "/home/libvirt/vols"
  virsh pool-build default
  virsh pool-start default
  virsh pool-autostart default

  # 重启kvm
  systemctl restart libvirtd
}

install_software() {
  install_tools
  install_docker
  install_kvm
}

load_agent() {
  local docker0
  docker0=$(ip addr | grep docker0 | grep inet | awk '{print $2}' | sed 's/\/16//')

  if [ -n "${global_docker_repo_username}" ]; then
    docker login http://${global_docker_repo_host}/ -u ${global_docker_repo_username} -p ${global_docker_repo_password}
  fi

  docker pull ${global_docker_repo_host}/virt_lib/virt_agent:latest
  docker image prune -f
  docker run --name agent \
    -e "JAVA_OPTS=-Duser.timezone=GMT+8
-DSSH_PWD=${global_ssh_password}
-DREDIS_HOST=${global_redis_host}
-DREDIS_PORT=${global_redis_port}
-DREDIS_PWD=${global_redis_password}
-DDOCKER0_IP=${docker0}
-DDB_URL=${global_db_host}
-DDB_USERNAME=${global_db_username}
-DDB_PASSWORD=${global_db_password}" \
    -v /home/libvirt/vols/:/home/libvirt/vols/ \
    -v /tmp/:/tmp/ \
    -v /etc/localtime:/etc/localtime:ro \
    --restart=always -d -p 8080:8080 ${global_docker_repo_host}/virt_lib/virt_agent:latest
}

main() {
  sys_init
  install_software
  load_agent
  reboot
}
main