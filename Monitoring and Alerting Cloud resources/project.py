import smtplib

import psutil
from azure.identity import AzureCliCredential, DefaultAzureCredential
from azure.mgmt.compute import ComputeManagementClient
from azure.mgmt.network import NetworkManagementClient
from azure.mgmt.resource import ResourceManagementClient

# Azure credentials and subscription ID
credential = AzureCliCredential()


# Resource group and virtual machine name
subscription_id = "b2e8af00-7b50-48d8-8b79-42ad54488baa"
resource_group_name = "IST615lab01"
vm_name = "projectVM"

# Initialize clients
compute_client = ComputeManagementClient(credential, subscription_id)
network_client = NetworkManagementClient(credential, subscription_id)
resource_client = ResourceManagementClient(credential, subscription_id)

# Getting virtual machine details
vm = compute_client.virtual_machines.get(resource_group_name, vm_name)

# Get virtual machine network details
network_interface_id = vm.network_profile.network_interfaces[0].id
network_interface = network_client.network_interfaces.get(
    resource_group_name, network_interface_id.split("/")[-1]
)

# Get virtual machine resource details
vm_resource = resource_client.resources.get_by_id(vm.id, api_version='2015-06-15')


print("Details of the VM:")
print(f"Virtual Machine Name: {vm.name}")
print()

#CPU Utlization
cpu_percent = psutil.cpu_percent(interval=1)
print(f"CPU utilization: {cpu_percent}%")

#Scaling up the VM
if cpu_percent>5:
    # Replace the value with the new VM size
    new_vm_size = "Standard_DS2_v2"

    # Create a ComputeManagementClient object
    credential = DefaultAzureCredential()
    compute_client = ComputeManagementClient(credential, subscription_id)

    # Get the VM object
    vm = compute_client.virtual_machines.get(resource_group_name, vm_name)

    # Update the VM size
    vm.hardware_profile.vm_size = new_vm_size

    # Update the VM
    compute_client.virtual_machines.begin_create_or_update(resource_group_name, vm_name, vm).wait()

    print(f"VM '{vm_name}' in resource group '{resource_group_name}' has been scaled up to '{new_vm_size}'.")
else:
    print(f"Scaling up of '{vm_name}' is not required since there is no traffic/demand")

print()
#Memory used
memory = psutil.virtual_memory()
print(f"Total Memory: {memory.total / (1024 * 1024)} MB")
print(f"Available Memory: {memory.available / (1024 * 1024)} MB")
print(f"Used Memory: {memory.used / (1024 * 1024)} MB")

memory_used_percentage = memory.used*100/memory.total
print(f"Memory used: {memory_used_percentage}%")
if(memory_used_percentage>15):
    # email content
    sender_email = "suprajanair@gmail.com"
    receiver_email = "soupsrnair@gmail.com"
    message = "This mail is to inform you that the Memory used has exceeded 15%"

    # connection to SMTP server
    smtp_server = "smtp.gmail.com"
    port = 587
    username = "suprajanair@gmail.com"
    password = "gnjhfytmgvxjnutl"
    smtp_conn = smtplib.SMTP(smtp_server, port)
    smtp_conn.starttls()
    smtp_conn.login(username, password)

    # send email
    smtp_conn.sendmail(sender_email, receiver_email, message)
    smtp_conn.quit()

    print("Email sent successfully.")
else:
    print("CPU Utilization is under threshold")


print()
#Disk Utilization
disk_usage = psutil.disk_usage('/')
print(f"Total disk space: {disk_usage.total}")
print(f"Used disk space: {disk_usage.used}")
print(f"Free disk space: {disk_usage.free}")
print(f"Disk utilization percentage: {disk_usage.percent}%")

if disk_usage.percent > 10:
    compute_client.virtual_machines.begin_power_off(resource_group_name, vm_name)   
    print(f"'{vm_name}' has been shut down")
else:
    pass
    print(f"No changes have been made to the status of '{vm_name}'")





