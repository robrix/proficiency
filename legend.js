var Hover = function (element, target) {
  this.handleEvent = event => {
    switch (event.type) {
    case 'mouseover':
      target.classList.add("active");
      break;
    case 'mouseout':
      target.classList.remove("active");
      break;
    case 'click':
      target.scrollIntoView();
      console.log("hello");
      break;
    }
  };
  element.addEventListener('mouseover', this);
  element.addEventListener('mouseout', this);
  element.addEventListener('click', this);
};

var Toggle = function(element, target) {
  this.handleEvent = event => {
    if (target.classList.contains("hidden")) {
      target.classList.remove("hidden");
    } else {
      target.classList.add("hidden");
    }
  };
  element.addEventListener('change', this);
};

function run() {
  for (let li of document.querySelectorAll("#legend li")) {
    let costCentreId = li.getAttribute("data-id");
    let path = document.querySelector("#path-" + costCentreId);
    li.hover = new Hover(li, path);
  }
  for (let path of document.querySelectorAll("#graph path")) {
    let costCentreId = path.getAttribute("data-id");
    let li = document.querySelector("#legend-" + costCentreId);
    path.hover = new Hover(path, li);
  }

  for (let input of document.querySelectorAll("#legend li input")) {
    let costCentreId = input.getAttribute("data-id");
    let path = document.querySelector("#path-" + costCentreId);
    input.toggle = new Toggle(input, path);
  }
}
