let noble = require("@abandonware/noble");

const ECHO_SERVICE_UUID = "ec00";
const ECHO_CHARACTERISTIC_UUID = "ec0e";

noble.on("discover", (peripheral) => {
  // connect to the first peripheral that is scanned
  noble.stopScanning();
  const name = peripheral.advertisement.localName;
  console.log(`Connecting to '${name}' ${peripheral.id}`);
  connectAndSetUp(peripheral);
});

function connectAndSetUp(peripheral) {
  peripheral.connect((error) => {
    console.log("Connected to", peripheral.id);

    // specify the services and characteristics to discover
    const serviceUUIDs = [ECHO_SERVICE_UUID];
    const characteristicUUIDs = [ECHO_CHARACTERISTIC_UUID];

    peripheral.discoverSomeServicesAndCharacteristics(
      serviceUUIDs,
      characteristicUUIDs,
      onServicesAndCharacteristicsDiscovered
    );
  });

  peripheral.on("disconnect", () => console.log("disconnected"));
}

function onServicesAndCharacteristicsDiscovered(
  error,
  services,
  characteristics
) {
  console.log("Discovered services and characteristics");
  const echoCharacteristic = characteristics[0];

  // create an interval to send data to the service
  let count = 0;
  setInterval(() => {
    count++;
    const message = Buffer.from("hello, 📈", "utf-8");
    console.log("Sending:  '" + message + "'");
    echoCharacteristic.write(message);
  }, 2500);
}

noble.on("stateChange", (state) => {
  if (state === "poweredOn") {
    console.log("Scanning");
    noble.startScanning([ECHO_SERVICE_UUID]);
  } else {
    noble.stopScanning();
  }
});
