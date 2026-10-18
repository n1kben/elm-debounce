const { Elm } = require("./integration.js");

const values = [];
const app = Elm.Main.init();

app.ports.observed.subscribe((value) => {
  values.push(value);
});

setTimeout(() => {
  const actual = values.slice().sort();
  const expected = ["immediate:now", "scheduled:second"];

  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    console.error("Expected", expected, "but observed", actual);
    process.exit(1);
  }

  console.log("Integration behavior passed:", actual.join(", "));
}, 100);
