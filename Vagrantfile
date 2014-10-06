# -*- mode: ruby -*-
# # vi: set ft=ruby :

CLOUD_CONFIG_PATH = "./cloud-config.yml"

Vagrant.configure("2") do |config|

  config.vm.box = "coreos-beta"
  config.vm.hostname = "consul-cluster"
  config.vm.network :private_network, ip: "192.168.150.10"

  # Provider specific configuration
  config.vm.provider :virtualbox do |virtualbox, override|
    override.vm.box_url = "http://beta.release.core-os.net/amd64-usr/current/coreos_production_vagrant.json"
    virtualbox.check_guest_additions = false
    virtualbox.memory = 1024
    virtualbox.cpus = 2
  end

  config.vm.provider :vmware_fusion do |vmware_fusion, override|
    override.vm.box_url = "http://beta.release.core-os.net/amd64-usr/current/coreos_production_vagrant_vmware_fusion.json"
    vmware_fusion.vmx["memsize"] = 1024
    vmware_fusion.vmx["numvcpus"] = 2
  end

  # Plugin conflict resolution
  if Vagrant.has_plugin?("vagrant-vbguest") then
    config.vbguest.auto_update = false
  end

  # Source folder for testing
  config.vm.synced_folder "../", "/home/core/share", id: "core", :nfs => true, :mount_options => ['nolock,vers=3,udp']

  config.vm.network "forwarded_port", guest: 2375, host: 2375
  config.vm.network "forwarded_port", guest: 5672, host: 5672
  config.vm.network "forwarded_port", guest: 15672, host: 15672

  # CoreOS startup
  config.vm.provision :file,
    :source => "#{CLOUD_CONFIG_PATH}",
    :destination => "/tmp/vagrantfile-user-data"
  config.vm.provision :shell,
    :inline => "mv /tmp/vagrantfile-user-data /var/lib/coreos-vagrant/",
    :privileged => true
end
