# Descargar la clave de firma
wget -q -O - https://packagecloud.io/slacktechnologies/slack/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/slack-keyring.gpg

# Agregar el repositorio
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/slack-keyring.gpg] https://packagecloud.io/slacktechnologies/slack/debian/ jessie main" | sudo tee /etc/apt/sources.list.d/slack.list

# Actualizar la lista de paquetes
sudo apt update

# Instalar Slack
sudo apt install slack-desktop
